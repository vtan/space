module App.Update.Logic where

import App.Prelude

import qualified App.Model.Body as Body
import qualified App.Model.BuildingTask as BuildingTask
import qualified App.Model.OrbitSystem as OrbitSystem
import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Ship as Ship
import qualified App.Model.ShipBuildingTask as ShipBuildingTask
import qualified App.UidMap as UidMap
import qualified Data.HashMap.Strict as HashMap

import App.Model.Body (Body(..))
import App.Model.BodyMinerals (BodyMinerals, Mineral, MineralData(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.Dims (AU)
import App.Model.GameState (GameState(..))
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Uid (Uid(..))
import App.Util (reduce)
import Data.String (fromString)

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
    & #bodyOrbitalStates .~ OrbitSystem.statesAtTime time (gs ^. #orbitSystem)
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
      let shipUid@(Uid shipNo) = gs ^. #ships & UidMap.nextUid
          ship = do
            position <- gs ^? #bodyOrbitalStates . at bodyUid . _Just . #position
            pure $ Ship
              { Ship.uid = shipUid
              , Ship.name = fromString $ "Ship " ++ show shipNo
              , Ship.position
              , Ship.speed = 1 / 1495970 -- 100 km/s
              , Ship.order = Nothing
              , Ship.attachedToBody = Just bodyUid
              }
      in gs
        & #colonies . at bodyUid . _Just . #shipBuildingTask .~ Nothing
        & #ships . at shipUid .~ ship
    _ -> gs

mineOnColony :: Uid Body -> Colony -> BodyMinerals -> GameState -> GameState
mineOnColony bodyUid Colony{ mines } minerals gs =
  let mineralsReservesMines = itoList $ HashMap.intersectionWith (,) minerals mines
  in gs & reduce mineralsReservesMines (\gs' (mineral, (MineralData{ available, accessibility }, mineQty)) ->
      let minedQty = min (fromIntegral mineQty * 0.01 * accessibility) available
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

addShip :: V2 (AU Double) -> GameState -> GameState
addShip pos gs =
  let shipNo = length (gs ^. #ships)
      shipUid = Uid shipNo
      ship = Ship
        { Ship.uid = shipUid
        , Ship.name = fromString $ "Ship " ++ show shipNo
        , Ship.position = pos
        , Ship.speed = 1 / 1495970 -- 100 km/s
        , Ship.order = Nothing
        , Ship.attachedToBody = Nothing
        }
  in gs & #ships . at shipUid .~ Just ship

moveShipToBody :: Ship -> Uid Body -> GameState -> GameState
moveShipToBody Ship{ Ship.uid, Ship.position, speed } bodyUid gs =
  let pathMay = PlottedPath.plot (gs ^. #time) position speed bodyUid (gs ^. #orbitSystem)
      orderMay = Ship.MoveToBody <$> pure bodyUid <*> pathMay
  in gs & #ships . at uid . _Just . #order .~ orderMay

cancelShipOrder :: Ship -> GameState -> GameState
cancelShipOrder Ship{ Ship.uid } gs =
  gs & #ships . at uid . _Just . #order .~ Nothing

foundColony :: Uid Body -> GameState -> GameState
foundColony bodyUid gs =
  gs & #colonies . at bodyUid .~ Just Colony{ stockpile = mempty, mines = mempty, buildingTask = Nothing, shipBuildingTask = Nothing }

startBuildingTask :: Uid Body -> Mineral -> GameState -> GameState
startBuildingTask bodyUid minedMineral gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    let buildingMaterials = colony ^. #stockpile . at 0 . non 0
        cost = 1
        finishTime = (gs ^. #time) + 30 * 24 * 3600
        newTask = BuildingTask{ minedMineral, BuildingTask.finishTime }
    in case colony ^. #buildingTask of
      Nothing | buildingMaterials >= cost ->
        colony
          & #stockpile . at 0 . non 0 -~ cost
          & #buildingTask .~ Just newTask
      _ -> colony

startShipBuildingTask :: Uid Body -> GameState -> GameState
startShipBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    let buildingMaterials = colony ^. #stockpile . at 0 . non 0
        cost = 2
        finishTime = (gs ^. #time) + 60 * 24 * 3600
        newTask = ShipBuildingTask{ ShipBuildingTask.finishTime }
    in case colony ^. #shipBuildingTask of
      Nothing | buildingMaterials >= cost ->
        colony
          & #stockpile . at 0 . non 0 -~ cost
          & #shipBuildingTask .~ Just newTask
      _ -> colony