module App.Model.GameState where

import App.Prelude

import App.Common.UidMap (UidMap)
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.Mineral (Mineral(..))
import App.Model.OrbitalState (OrbitalState)
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))

data GameState = GameState
  { rootBody :: Body
  , bodies :: UidMap Body Body
  , bodyOrbitalStates :: UidMap Body OrbitalState
  , bodyMinerals :: UidMap Body (HashMap Resource Mineral)
  , colonies :: UidMap Body Colony
  , ships :: UidMap Ship Ship
  , time :: Time Int
  }
  deriving (Show, Generic)
