module App.Logic.Time where

import App.Prelude

import qualified App.Common.UidMap as UidMap
import qualified App.Dimension.Time as Time
import qualified App.Logic.Colony as Logic.Colony
import qualified App.Model.Body as Body
import qualified App.Model.BuildingTask as BuildingTask
import qualified App.Model.Installation as Installation
import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Model.ShipBuildingTask as ShipBuildingTask

import App.Common.Uid (Uid(..))
import App.Common.Util (reduce)
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))

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

jumpToNextMidnight :: GameState -> GameState
jumpToNextMidnight gs@GameState{ time } =
  gs & stepTime (Time.nextMidnight time - time)

jumpTimeTo :: Time Int -> GameState -> GameState
jumpTimeTo time gs =
  gs
    & #time .~ time
    & #bodyOrbitalStates .~ Body.statesAtTime time (gs ^. #rootBody)
    & (\gs' -> gs' & #ships . traversed %~ updateShip gs')

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
            pure $ Logic.Colony.shipBuiltAt bodyUid orbitalState shipUid
      in gs
        & #colonies . at bodyUid . _Just . #shipBuildingTask .~ Nothing
        & #ships . at shipUid .~ ship
    Just ShipBuildingTask{} -> gs
    Nothing -> gs

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
    maxPopulation <- Logic.Colony.colonyMaxPopulation body colony
    pure $
      if population > maxPopulation
      then gs & #colonies . at bodyUid . _Just . #population .~ (population + maxPopulation) `div` 2
      else gs
