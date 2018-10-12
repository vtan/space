module App.Model.OrbitalState where

import App.Prelude

import App.Dimension.Local (Local)

data OrbitalState = OrbitalState
  { position :: V2 (Local Double)
  , orbitCenter :: V2 (Local Double)
  }
  deriving (Show, Generic)
