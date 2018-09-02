module App.Model.OrbitalState where

import App.Prelude

import App.Model.Dims (AU)

data OrbitalState = OrbitalState
  { position :: V2 (AU Double)
  , orbitCenter :: V2 (AU Double)
  }
  deriving (Show, Generic)