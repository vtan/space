module Game.Colonies.BuildingLogic where

import GlobalImports

import qualified Game.GameState as GameState
import qualified Game.Bodies.Resource as Resource
import qualified Game.Bodies.ResourceOnBody as ResourceOnBody

import Game.GameState (GameState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.Resource (Resource)
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Colonies.Building (Building(..))
import Game.Colonies.BuildOrder (BuildOrder(..))
import Game.Colonies.Colony (Colony(..))
import Game.Common.Id (Id)

resourceCostFor :: Building -> HashMap Resource Double
resourceCostFor _ = [(Resource.Resource1, 5)]

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

cancel :: Id Body -> GameState -> GameState
cancel = undefined

onProductionTick :: Id Body -> GameState -> GameState
onProductionTick = undefined
