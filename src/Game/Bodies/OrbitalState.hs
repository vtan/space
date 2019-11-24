module Game.Bodies.OrbitalState where

import GlobalImports

import Game.Dimension.Local (Local)

data OrbitalState = OrbitalState
  { position :: V2 (Local Double)
  , orbitCenter :: V2 (Local Double)
  }
  deriving (Show, Generic)
