module Game.Colonies.BuildingLogic where

import GlobalImports

import qualified Game.GameState as GameState
import qualified Game.Bodies.Resource as Resource
import qualified Game.Colonies.Building as Building

import Game.GameState (GameState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.Resource (Resource)
import Game.Colonies.Building (Building(..))
import Game.Colonies.BuildOrder (BuildOrder(..))
import Game.Colonies.Colony (Colony(..))
import Game.Common.Id (Id)

import Linear ((^+^))

resourceCostFor :: Building -> HashMap Resource Double
resourceCostFor _ = [(Resource.Resource1, 5)]

buildEffortFor :: Building -> Int
buildEffortFor _ = 100

buildEffortPerTick :: Colony -> Int
buildEffortPerTick Colony{ buildings } =
  let factories = view (at Building.Factory . non 0) buildings
  in 2 * factories

remainingTicksToComplete :: Colony -> BuildOrder -> Maybe Int
remainingTicksToComplete colony BuildOrder{ target, spentBuildEffort } =
  let remainingBuildEffort = buildEffortFor target - spentBuildEffort
  in
    case buildEffortPerTick colony of
      0 -> Nothing
      perTick -> Just (ceiling (fromIntegral remainingBuildEffort / fromIntegral perTick :: Double))

start :: Building -> Int -> Id Body -> GameState -> GameState
start target quantity bodyId gs =
  let
    colony = GameState.getColony bodyId gs
    availableResources = view #resources colony
    spentResources = resourceCostFor target
    order = BuildOrder{ target, quantity, spentResources, spentBuildEffort = 0 }
    colony' = Resource.spend availableResources spentResources & fmap \remainingResources ->
      colony{ buildOrder = Just order, resources = remainingResources }
  in
    case colony' of
      Just c -> gs & set (#colonies . at bodyId . _Just) c
      Nothing -> gs

cancel :: BuildOrder -> Id Body -> GameState -> GameState
cancel BuildOrder{ spentResources } bodyId =
  over (#colonies . at bodyId . _Just) \colony@Colony{ resources } ->
    colony{ buildOrder = Nothing, resources = resources ^+^ spentResources }

onProductionTick :: Id Body -> GameState -> GameState
onProductionTick = undefined
