module App.Update.Logic where

import App.Prelude

import qualified App.Dimension.Speed as Speed
import qualified App.Dimension.Time as Time
import qualified App.Model.Body as Body
import qualified App.Model.BuildingTask as BuildingTask
import qualified App.Model.Installation as Installation
import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Model.ShipBuildingTask as ShipBuildingTask
import qualified App.UidMap as UidMap

import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Mineral (Mineral(..))
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Uid (Uid(..))
import App.Util (reduce)
import Data.String (fromString)

jumpToNextMidnight :: GameState -> GameState
jumpToNextMidnight gs@GameState{ time } =
  gs & stepTime (Time.nextMidnight time - time)

stepTime :: Time Int -> GameState -> GameState
stepTime dt gs =
  let now = gs ^. #time
      untilTick = Time.nextMidnight now - now
  in if
    | dt == 0 -> gs
    | dt < untilTick -> gs & jumpTimeTo (now + dt)
    | otherwise -> gs
        & jumpTimeTo (now + untilTick)
        & productionTick
        & stepTime (dt - untilTick)

jumpTimeTo :: Time Int -> GameState -> GameState
jumpTimeTo time gs =
  gs
    & #time .~ time
    & #bodyOrbitalStates .~ Body.statesAtTime time (gs ^. #rootBody)
    & (\gs' -> gs' & #ships . traversed %~ updateShip gs')

productionTick :: GameState -> GameState
productionTick gs@GameState{ colonies } =
  UidMap.keys colonies
    & foldl' (flip productionTickOnColony) gs

productionTickOnColony :: Uid Body -> GameState -> GameState
productionTickOnColony bodyUid =
  buildOnColony bodyUid
  >>> buildShipOnColony bodyUid
  >>> mineOnColony bodyUid
  >>> shrinkPopulation bodyUid

buildOnColony :: Uid Body -> GameState -> GameState
buildOnColony bodyUid gs@GameState{ colonies, time } =
  case colonies ^? at bodyUid . _Just . #buildingTask . _Just of
    Just BuildingTask{ installation, quantity, BuildingTask.finishTime } | finishTime <= time ->
      gs & #colonies . at bodyUid . _Just %~ (\col ->
          col
            & #stockpile . at (Resource.Installation installation) . non 0 +~ quantity
            & #buildingTask .~ Nothing
        )
    Just BuildingTask{} -> gs
    Nothing -> gs

buildShipOnColony :: Uid Body -> GameState -> GameState
buildShipOnColony bodyUid gs@GameState{ colonies, time } =
  case colonies ^? at bodyUid . _Just . #shipBuildingTask . _Just of
    Just ShipBuildingTask{ ShipBuildingTask.finishTime } | finishTime <= time ->
      let shipUid = gs ^. #ships & UidMap.nextUid
          ship = do
            orbitalState <- gs ^. #bodyOrbitalStates . at bodyUid
            pure $ shipBuiltAt bodyUid orbitalState shipUid
      in gs
        & #colonies . at bodyUid . _Just . #shipBuildingTask .~ Nothing
        & #ships . at shipUid .~ ship
    Just ShipBuildingTask{} -> gs
    Nothing -> gs

shipBuiltAt :: Uid Body -> OrbitalState -> Uid Ship -> Ship
shipBuiltAt bodyUid OrbitalState{ position } shipUid@(Uid shipNo) =
  Ship
    { Ship.uid = shipUid
    , Ship.name = fromString $ "Ship " ++ show shipNo
    , Ship.position = position
    , Ship.cargoCapacity = 1000
    , Ship.loadedCargo = mempty
    , Ship.cabinCapacity = 1000
    , Ship.loadedPopulation = 0
    , Ship.speed = Speed.kmPerSec 100
    , Ship.order = Nothing
    , Ship.attachedToBody = Just bodyUid
    }

mineOnColony :: Uid Body -> GameState -> GameState
mineOnColony bodyUid gs@GameState{ colonies, bodyMinerals } =
  fromMaybe gs $ do
    Colony{ installations } <- colonies ^. at bodyUid
    minerals <- bodyMinerals ^. at bodyUid
    let mines = installations ^. at Installation.Mine . non 0
        mineralsReserves = itoList minerals
    pure $
      gs & reduce mineralsReserves (\gs' (mineral, Mineral{ available, accessibility }) ->
          let minedQty = min (floor (10 * accessibility * fromIntegral mines / 1000)) available
          in gs'
            & #bodyMinerals . at bodyUid . _Just . at mineral . _Just . #available -~ minedQty
            & #colonies . at bodyUid . _Just . #stockpile . at mineral . non 0 +~ minedQty
        )

shrinkPopulation :: Uid Body -> GameState -> GameState
shrinkPopulation bodyUid gs =
  fromMaybe gs $ do
    body <- gs ^. #bodies . at bodyUid
    colony@Colony{ population } <- gs ^. #colonies . at bodyUid
    maxPopulation <- colonyMaxPopulation body colony
    pure $
      if population > maxPopulation
      then gs & #colonies . at bodyUid . _Just . #population .~ (population + maxPopulation) `div` 2
      else gs

updateShip :: GameState -> Ship -> Ship
updateShip gs ship =
  case ship ^. #order of
    Just Ship.MoveToBody{ Ship.path, Ship.bodyUid } ->
      let now = gs ^. #time
          arrived = now >= path ^. #endTime
      in ship
        & #position .~ (path & PlottedPath.atTime now)
        & (if arrived then #order .~ Nothing else id)
        & #attachedToBody .~ (if arrived then Just bodyUid else Nothing)
    Nothing ->
      case ship ^. #attachedToBody >>= (\b -> gs ^? #bodyOrbitalStates . at b . _Just . #position) of
        Just position ->
          ship & #position .~ position
        Nothing -> ship

moveShipToBody :: Ship -> Uid Body -> GameState -> GameState
moveShipToBody Ship{ Ship.uid, Ship.position, speed } bodyUid gs =
  let pathMay = PlottedPath.plot (gs ^. #time) position speed bodyUid (gs ^. #rootBody)
      orderMay = Ship.MoveToBody <$> pure bodyUid <*> pathMay
  in gs & #ships . at uid . _Just . #order .~ orderMay

cancelShipOrder :: Ship -> GameState -> GameState
cancelShipOrder Ship{ Ship.uid } gs =
  gs & #ships . at uid . _Just . #order .~ Nothing

loadResourceToShip :: Maybe Int -> Resource -> Ship -> GameState -> GameState
loadResourceToShip qtyOrAll resource Ship{ Ship.uid = shipUid, attachedToBody, cargoCapacity, loadedCargo } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    availableOnColony <- gs ^? #colonies . at bodyUid . _Just . #stockpile . at resource . _Just
    let remainingCargoSpace = cargoCapacity - sum loadedCargo
    let loadedQty = minimum (toList qtyOrAll ++ [availableOnColony, remainingCargoSpace])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedCargo . at resource . non 0 +~ loadedQty
      & #colonies . at bodyUid . _Just . #stockpile . at resource . non 0 -~ loadedQty

unloadResourceFromShip :: Maybe Int -> Resource -> Ship -> GameState -> GameState
unloadResourceFromShip qtyOrAll resource Ship{ Ship.uid = shipUid, attachedToBody, loadedCargo } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    _ <- gs ^. #colonies . at bodyUid
    availableOnShip <- loadedCargo ^. at resource
    let unloadedQty = minimum (toList qtyOrAll ++ [availableOnShip])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedCargo . at resource . non 0 -~ unloadedQty
      & #colonies . at bodyUid . _Just . #stockpile . at resource . non 0 +~ unloadedQty

loadPopulationToShip :: Maybe Int -> Ship -> GameState -> GameState
loadPopulationToShip qtyOrAll Ship{ Ship.uid = shipUid, attachedToBody, cabinCapacity, loadedPopulation } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    populationOnColony <- gs ^? #colonies . at bodyUid . _Just . #population
    let remainingCabinSpace = cabinCapacity - loadedPopulation
    let loadedQty = minimum (toList qtyOrAll ++ [populationOnColony, remainingCabinSpace])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedPopulation +~ loadedQty
      & #colonies . at bodyUid . _Just . #population -~ loadedQty

unloadPopulationFromShip :: Maybe Int -> Ship -> GameState -> GameState
unloadPopulationFromShip qtyOrAll Ship{ Ship.uid = shipUid, attachedToBody, loadedPopulation } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    _ <- gs ^. #colonies . at bodyUid
    let unloadedQty = minimum (toList qtyOrAll ++ [loadedPopulation])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedPopulation -~ unloadedQty
      & #colonies . at bodyUid . _Just . #population +~ unloadedQty

foundColony :: Uid Body -> GameState -> GameState
foundColony bodyUid gs =
  gs & #colonies . at bodyUid .~ Just Colony
    { population = 0
    , isHomeworld = False
    , stockpile = mempty
    , installations = mempty
    , buildingTask = Nothing
    , shipBuildingTask = Nothing
    }

startBuildingTask :: Uid Body -> Installation -> GameState -> GameState
startBuildingTask bodyUid installation gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    let buildingMaterials = colony ^. #stockpile . at Resource.Mineral . non 0
        cost = 1000
        quantity = 500
        finishTime = (gs ^. #time) + 30 * 24 * 3600
        newTask = BuildingTask{ installation, quantity, BuildingTask.finishTime }
    in case colony ^. #buildingTask of
      Nothing | buildingMaterials >= cost ->
        colony
          & #stockpile . at Resource.Mineral . non 0 -~ cost
          & #buildingTask .~ Just newTask
      _ -> colony

cancelBuildingTask :: Uid Body -> GameState -> GameState
cancelBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just . #buildingTask .~ Nothing

startShipBuildingTask :: Uid Body -> GameState -> GameState
startShipBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    let buildingMaterials = colony ^. #stockpile . at Resource.Mineral . non 0
        cost = 2000
        finishTime = (gs ^. #time) + 1 * 24 * 3600
        newTask = ShipBuildingTask{ ShipBuildingTask.finishTime }
    in case colony ^. #shipBuildingTask of
      Nothing | buildingMaterials >= cost ->
        colony
          & #stockpile . at Resource.Mineral . non 0 -~ cost
          & #shipBuildingTask .~ Just newTask
      _ -> colony

cancelShipBuildingTask :: Uid Body -> GameState -> GameState
cancelShipBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just . #shipBuildingTask .~ Nothing

installInstallation :: Installation -> Int -> Uid Body -> Colony -> GameState -> GameState
installInstallation installation qty bodyUid colony gs =
  fromMaybe gs $ do
    availableQty <- colony ^. #stockpile . at (Resource.Installation installation)
    let installedQty = min qty availableQty
        colony' = colony
          & #installations . at installation . non 0 +~ installedQty
          & #stockpile . at (Resource.Installation installation) . non 0 -~ installedQty
    pure $ gs
      & #colonies . at bodyUid .~ Just colony'

uninstallInstallation :: Installation -> Int -> Uid Body -> Colony -> GameState -> GameState
uninstallInstallation installation qty bodyUid colony gs =
  fromMaybe gs $ do
    installedQty <- colony ^. #installations . at installation
    let uninstalledQty = min qty installedQty
        colony' = colony
          & #stockpile . at (Resource.Installation installation) . non 0 +~ uninstalledQty
          & #installations . at installation . non 0 -~ uninstalledQty
    pure $ gs
      & #colonies . at bodyUid .~ Just colony'

colonyMaxPopulation :: Body -> Colony -> Maybe Int
colonyMaxPopulation Body{ colonyCost } Colony{ isHomeworld, installations } =
  case colonyCost of
    _ | isHomeworld -> Nothing
    Nothing -> Just 0
    Just cc ->
      let installationQty = installations ^. at Installation.Infrastructure . non 0
      in Just $ floor (10 / cc * fromIntegral installationQty)
