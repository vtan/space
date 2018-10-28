module App.Model.GameState where

import App.Prelude

import App.Common.IdMap (IdMap)
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.Mineral (Mineral(..))
import App.Model.OrbitalState (OrbitalState)
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))

data GameState = GameState
  { rootBody :: Body
  , bodies :: IdMap Body Body
  , bodyOrbitalStates :: IdMap Body OrbitalState
  , bodyMinerals :: IdMap Body (HashMap Resource Mineral)
  , colonies :: IdMap Body Colony
  , ships :: IdMap Ship Ship
  , time :: Time Int
  }
  deriving (Show, Generic)
