module App.Logic.TimeStep
  ( stepTime, jumpToNextMidnight )
where

import App.Prelude

import qualified App.Common.IdMap as IdMap
import qualified App.Dimension.Time as Time
import qualified App.Logic.Building as Logic.Building
import qualified App.Logic.Mining as Logic.Mining
import qualified App.Model.Body as Body
import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Ship as Ship

import App.Common.Id (Id(..))
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.GameState (GameState(..))
import App.Model.Ship (Ship(..))

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
    Just Ship.MoveToBody{ Ship.path, Ship.bodyId } ->
      let now = gs ^. #time
          arrived = now >= path ^. #endTime
      in ship
        & #position .~ (path & PlottedPath.atTime now)
        & (if arrived then #order .~ Nothing else id)
        & #attachedToBody .~ (if arrived then Just bodyId else Nothing)
    Nothing ->
      case ship ^. #attachedToBody >>= (\b -> gs ^? #bodyOrbitalStates . at b . _Just . #position) of
        Just position ->
          ship & #position .~ position
        Nothing -> ship

productionTick :: GameState -> GameState
productionTick gs@GameState{ colonies } =
  IdMap.keys colonies
    & foldl' (flip productionTickOnColony) gs

productionTickOnColony :: Id Body -> GameState -> GameState
productionTickOnColony bodyId =
  Logic.Building.build bodyId
  >>> Logic.Mining.mine bodyId
