module Game.TimeLogic
  ( stepTime, jumpToNextMidnight )
where

import GlobalImports

import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Ship as Ship
import qualified Game.Bodies.OrbitTree as OrbitTree
import qualified Game.Colonies.MiningLogic as MiningLogic
import qualified Game.Common.IdMap as IdMap
import qualified Game.Dimension.Time as Time

import App.Model.Ship (Ship(..))
import Game.GameState (GameState(..))
import Game.Bodies.Body (Body)
import Game.Common.Id (Id(..))
import Game.Dimension.Time (Time)

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
    & #bodyOrbitalStates .~ OrbitTree.statesAtTime time (gs ^. #orbitTree)
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
  MiningLogic.mineOnColony bodyId
