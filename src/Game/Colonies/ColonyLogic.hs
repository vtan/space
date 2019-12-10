module Game.Colonies.ColonyLogic where

import GlobalImports

import Game.GameState (GameState(..))
import Game.Bodies.Body (Body(..))
import Game.Colonies.Building (Building(..))
import Game.Colonies.Colony (Colony(..))
import Game.Common.Id (Id)

foundColony :: Id Body -> Int -> GameState -> GameState
foundColony bodyId initialFactories gs =
  let colony = Colony
        { bodyId
        , resources = mempty
        , buildings = [(Factory, initialFactories)]
        , buildOrder = Nothing
        }
  in gs & set (#colonies . at bodyId) (Just colony)
