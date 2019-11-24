module Game.GameState where

import GlobalImports

import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitalState (OrbitalState)
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Colonies.Colony (Colony(..))
import Game.Bodies.Resource (Resource)
import Game.Common.IdMap (IdMap)
import Game.Dimension.Time (Time)
import App.Model.Ship (Ship(..))

data GameState = GameState
  { rootBody :: Body
  , bodies :: IdMap Body Body
  , bodyOrbitalStates :: IdMap Body OrbitalState
  , bodyMinerals :: IdMap Body (HashMap Resource ResourceOnBody)
  , colonies :: IdMap Body Colony
  , ships :: IdMap Ship Ship
  , time :: Time Int
  }
  deriving (Show, Generic)
