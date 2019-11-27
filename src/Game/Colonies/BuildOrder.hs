module Game.Colonies.BuildOrder where

import GlobalImports

import Game.Bodies.Resource (Resource)
import Game.Colonies.Building (Building)

data BuildOrder = BuildOrder
  { target :: Building
  , quantity :: Int
  , spentResources :: HashMap Resource Double
  , spentBuildEffort :: Double
  }
  deriving (Generic, Show)
