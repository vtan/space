module Game.Colonies.BuildOrder where

import GlobalImports

import Game.Bodies.Resource (Resource)
import Game.Colonies.Building (Building)

data BuildOrder = BuildOrder
  { target :: Building
  , quantity :: Int
  , lockedResources :: HashMap Resource Double
  , lockedResourcesPerQuantity :: HashMap Resource Double
  , spentBuildEffort :: Int
  }
  deriving (Generic, Show)
