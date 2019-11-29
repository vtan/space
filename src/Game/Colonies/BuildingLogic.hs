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

import Linear ((^+^), (^-^))

resourceCostFor :: Building -> HashMap Resource Double
resourceCostFor _ = [(Resource.Resource1, 5)]

buildEffortFor :: Building -> Int -> Int
buildEffortFor _ quantity = quantity * 100

buildEffortPerTick :: Colony -> Int
buildEffortPerTick Colony{ buildings } =
  let factories = view (at Building.Factory . non 0) buildings
  in 2 * factories

ticksToBuild :: Colony -> Building -> Int -> Maybe Int
ticksToBuild colony building quantity =
  case buildEffortPerTick colony of
    0 -> Nothing
    perTick ->
      let effort = buildEffortFor building quantity
      in Just (ceiling (fromIntegral effort / fromIntegral perTick :: Double))

remainingTicksToComplete :: Colony -> BuildOrder -> Maybe Int
remainingTicksToComplete colony BuildOrder{ target, quantity, spentBuildEffort } =
  let remainingBuildEffort = buildEffortFor target quantity - spentBuildEffort
  in
    case buildEffortPerTick colony of
      0 -> Nothing
      perTick -> Just (ceiling (fromIntegral remainingBuildEffort / fromIntegral perTick :: Double))

start :: Building -> Int -> Id Body -> GameState -> GameState
start target quantity bodyId gs =
  let
    colony = GameState.expectColony bodyId gs
    availableResources = view #resources colony
    lockedResourcesPerQuantity = resourceCostFor target
    lockedResources = fromIntegral quantity *^ lockedResourcesPerQuantity
    order = BuildOrder{ target, quantity, lockedResources, lockedResourcesPerQuantity, spentBuildEffort = 0 }
    colony' = Resource.spend availableResources lockedResources & fmap \remainingResources ->
      colony{ buildOrder = Just order, resources = remainingResources }
  in
    case colony' of
      Just c -> gs & set (#colonies . at bodyId . _Just) c
      Nothing -> gs

cancel :: BuildOrder -> Id Body -> GameState -> GameState
cancel BuildOrder{ lockedResources } bodyId =
  over (#colonies . at bodyId . _Just) \colony@Colony{ resources } ->
    colony{ buildOrder = Nothing, resources = resources ^+^ lockedResources }

onProductionTick :: Id Body -> GameState -> GameState
onProductionTick bodyId gs =
  let colony@Colony{ buildOrder, buildings } = GameState.expectColony bodyId gs
  in
    case buildOrder of
      Nothing -> gs
      Just order@BuildOrder{ target, quantity, spentBuildEffort, lockedResources, lockedResourcesPerQuantity } ->
        let
          (finishedBuildings, buildEffortNow) =
            (spentBuildEffort + buildEffortPerTick colony) `divMod` buildEffortFor target 1
          colony' =
            if finishedBuildings >= quantity
            then colony
              { buildOrder = Nothing
              , buildings = over (at target . non 0) (+ quantity) buildings
              }
            else if finishedBuildings > 0
            then colony
              { buildOrder = Just order
                  { spentBuildEffort = buildEffortNow
                  , quantity = quantity - finishedBuildings
                  , lockedResources = lockedResources ^-^ lockedResourcesPerQuantity
                  }
              , buildings = over (at target . non 0) (+ finishedBuildings) buildings
              }
            else colony{ buildOrder = Just order{ spentBuildEffort = buildEffortNow } }
        in
          gs & set (#colonies . at bodyId . _Just) colony'

