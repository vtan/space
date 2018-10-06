module App.Model.GameState where

import App.Prelude

import App.Camera (Camera(..))
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.Dims (AU)
import App.Model.Mineral (Mineral(..))
import App.Model.OrbitalState (OrbitalState)
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.UidMap (UidMap)

data GameState = GameState
  { rootBody :: Body
  , bodies :: UidMap Body Body
  , bodyOrbitalStates :: UidMap Body OrbitalState
  , bodyMinerals :: UidMap Body (HashMap Resource Mineral)
  , colonies :: UidMap Body Colony
  , ships :: UidMap Ship Ship
  , time :: Int
  , timeStepPerFrame :: Maybe Int
  , movingViewport :: Bool
  , camera :: Camera (AU Double) Double
  }
  deriving (Show, Generic)
