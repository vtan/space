module App.Update.Logic where

import App.Prelude

import qualified App.Model.Body as Body
import qualified App.Model.BuildingTask as BuildingTask
import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Model.ShipBuildingTask as ShipBuildingTask
import qualified App.UidMap as UidMap
import qualified Data.HashMap.Strict as HashMap

import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Uid (Uid(..))
import App.Util (reduce)
import Data.String (fromString)

setGameSpeed :: Int -> GameState -> GameState
setGameSpeed speed gs =
  -- TODO assuming 60 fps
  case speed of
    0 -> gs & #timeStepPerFrame .~ Nothing
    1 -> gs & #timeStepPerFrame .~ Just 1
    2 -> gs & #timeStepPerFrame .~ Just 10
    3 -> gs & #timeStepPerFrame .~ Just 60
    4 -> gs & #timeStepPerFrame .~ Just (12 * 60)
    5 -> gs & #timeStepPerFrame .~ Just (5 * 24 * 60)
    _ -> gs

jumpToNextMidnight :: GameState -> GameState
jumpToNextMidnight gs
  | gs ^. #timeStepPerFrame & has _Nothing =
    let now = gs ^. #time
    in gs & stepTime (timeUntilNextMidnight now)
  | otherwise = gs

stepTime :: Int -> GameState -> GameState
stepTime dt gs =
  let now = gs ^. #time
      untilTick = timeUntilNextMidnight now
  in if
    | dt == 0 -> gs
    | dt < untilTick -> gs & jumpTimeTo (now + dt)
    | otherwise -> gs
        & jumpTimeTo (now + untilTick)
        & productionTick
        & stepTime (dt - untilTick)

jumpTimeTo :: Int -> GameState -> GameState
jumpTimeTo time gs =
  gs
    & #time .~ time
    & #bodyOrbitalStates .~ Body.statesAtTime time (gs ^. #rootBody)
    & (\gs' -> gs' & #ships . traversed %~ updateShip gs')

timeUntilNextMidnight :: Int -> Int
timeUntilNextMidnight now = (quot now 86400 + 1) * 86400 - now

productionTick :: GameState -> GameState
productionTick gs@GameState{ bodies } =
  gs & reduce bodies (\gs' Body{ Body.uid } ->
      let colony = gs' ^. #colonies . at uid
          minerals = gs' ^. #bodyMinerals . at uid
          build = (buildOnColony uid <$> colony) & fromMaybe id
          buildShip = (buildShipOnColony uid <$> colony) & fromMaybe id
          mine = (mineOnColony uid <$> colony <*> minerals) & fromMaybe id
      in gs' & (build >>> buildShip >>> mine)
    )

buildOnColony :: Uid Body -> Colony -> GameState -> GameState
buildOnColony bodyUid col@Colony{ buildingTask } gs@GameState{ time } =
  case buildingTask of
    Just BuildingTask{ minedMineral, BuildingTask.finishTime } | finishTime <= time ->
      let col' = col
            & #mines . at minedMineral . non 0 +~ 1
            & #buildingTask .~ Nothing
      in gs & #colonies . at bodyUid . _Just .~ col'
    _ -> gs

buildShipOnColony :: Uid Body -> Colony -> GameState -> GameState
buildShipOnColony bodyUid Colony{ shipBuildingTask } gs@GameState{ time } =
  case shipBuildingTask of
    Just ShipBuildingTask{ ShipBuildingTask.finishTime } | finishTime <= time ->
      let shipUid = gs ^. #ships & UidMap.nextUid
          ship = do
            orbitalState <- gs ^. #bodyOrbitalStates . at bodyUid
            pure $ shipBuiltAt bodyUid orbitalState shipUid
      in gs
        & #colonies . at bodyUid . _Just . #shipBuildingTask .~ Nothing
        & #ships . at shipUid .~ ship
    _ -> gs

shipBuiltAt :: Uid Body -> OrbitalState -> Uid Ship -> Ship
shipBuiltAt bodyUid OrbitalState{ position } shipUid@(Uid shipNo) =
  Ship
    { Ship.uid = shipUid
    , Ship.name = fromString $ "Ship " ++ show shipNo
    , Ship.position = position
    , Ship.cargoCapacity = 1000
    , Ship.loadedCargo = mempty
    , Ship.speed = 1 / 1495970 -- 100 km/s
    , Ship.order = Nothing
    , Ship.attachedToBody = Just bodyUid
    }

mineOnColony :: Uid Body -> Colony -> HashMap Resource Mineral -> GameState -> GameState
mineOnColony bodyUid Colony{ mines } minerals gs =
  let mineralsReservesMines = itoList $ HashMap.intersectionWith (,) minerals mines
  in gs & reduce mineralsReservesMines (\gs' (mineral, (Mineral{ available, accessibility }, mineQty)) ->
      let minedQty = min (mineQty * floor (10 * accessibility)) available
      in gs'
        & #bodyMinerals . at bodyUid . _Just . at mineral . _Just . #available -~ minedQty
        & #colonies . at bodyUid . _Just . #stockpile . at mineral . non 0 +~ minedQty
    )

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

foundColony :: Uid Body -> GameState -> GameState
foundColony bodyUid gs =
  gs & #colonies . at bodyUid .~ Just Colony{ stockpile = mempty, mines = mempty, buildingTask = Nothing, shipBuildingTask = Nothing }

startBuildingTask :: Uid Body -> Resource -> GameState -> GameState
startBuildingTask bodyUid minedMineral gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    let buildingMaterials = colony ^. #stockpile . at Resource.Mineral . non 0
        cost = 1000
        finishTime = (gs ^. #time) + 30 * 24 * 3600
        newTask = BuildingTask{ minedMineral, BuildingTask.finishTime }
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
