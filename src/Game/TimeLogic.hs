module Game.TimeLogic
  ( stepTime, jumpToNextMidnight )
where

import GlobalImports

import qualified Game.Bodies.OrbitTree as OrbitTree
import qualified Game.Colonies.BuildingLogic as BuildingLogic
import qualified Game.Colonies.MiningLogic as MiningLogic
import qualified Game.Common.IdMap as IdMap
import qualified Game.Dimension.Time as Time
import qualified Game.Ships.ShipLogic as ShipLogic

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
jumpTimeTo time gs@GameState{ ships } =
  gs
    & #time .~ time
    & #bodyOrbitalStates .~ OrbitTree.statesAtTime time (gs ^. #orbitTree)
    & (\gs' -> foldl' (flip ShipLogic.update) gs' ships)

productionTick :: GameState -> GameState
productionTick gs@GameState{ colonies } =
  IdMap.keys colonies
    & foldl' (flip productionTickOnColony) gs

productionTickOnColony :: Id Body -> GameState -> GameState
productionTickOnColony bodyId =
  MiningLogic.onProductionTick bodyId
  >>> BuildingLogic.onProductionTick bodyId
