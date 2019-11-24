module Game.GameState where

import GlobalImports

import Game.Bodies.Body (Body)
import Game.Bodies.OrbitTree (OrbitTree)
import Game.Bodies.OrbitalState (OrbitalState)
import Game.Colonies.Colony (Colony)
import Game.Common.IdMap (IdMap)
import Game.Dimension.Time (Time)
import App.Model.Ship (Ship)

data GameState = GameState
  { orbitTree :: OrbitTree
  , bodies :: IdMap Body Body
  , bodyOrbitalStates :: IdMap Body OrbitalState
  , colonies :: IdMap Body Colony
  , ships :: IdMap Ship Ship
  , time :: Time Int
  }
  deriving (Show, Generic)
