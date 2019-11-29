module Game.Colonies.MiningLogic where

import GlobalImports

import qualified Game.GameState as GameState
import qualified Game.Bodies.Resource as Resource
import qualified Game.Bodies.ResourceOnBody as ResourceOnBody

import Game.GameState (GameState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Colonies.Building (Building(Mine))
import Game.Colonies.Colony (Colony(..))
import Game.Common.Id (Id)

dailyOutput :: Int -> ResourceOnBody -> Double
dailyOutput mines ResourceOnBody{ available, accessibility } =
  min available (accessibility * 10 * fromIntegral mines)

onProductionTick :: Id Body -> GameState -> GameState
onProductionTick bodyId gs =
  foldl'
    ( \acc resource ->
        let
          Colony{ buildings } = GameState.expectColony bodyId gs
          Body{ resources } = GameState.expectBody bodyId gs
          mines = view (at (Mine resource) . non 0) buildings
          resourceOnBody = fromMaybe ResourceOnBody.empty $ view (at resource) resources
          mined = dailyOutput mines resourceOnBody
        in
          if mined > 0
          then
            acc
              & over
                  (#colonies . at bodyId . _Just . #resources . at resource . non 0)
                  (+ mined)
              & over
                  (#bodies . at bodyId . _Just . #resources . at resource . _Just . #available)
                  (subtract mined)
          else
            acc
    )
    gs
    Resource.all
