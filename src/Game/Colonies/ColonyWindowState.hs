module Game.Colonies.ColonyWindowState
  ( ColonyWindowState(..)
  , initial
  )
where

import GlobalImports

import Game.Bodies.Body (Body)
import Game.Colonies.Building (Building)
import Game.Common.Id (Id)

data ColonyWindowState = ColonyWindowState
  { selectedBodyId :: Maybe (Id Body)
  , selectedBodyIdScroll :: Double
  , selectedBuilding :: Maybe (Building)
  , selectedBuildingScroll :: Double
  , buildingQuantity :: Int
  , buildingQuantityScroll :: Double
  }
  deriving (Generic)

initial :: ColonyWindowState
initial = ColonyWindowState
  { selectedBodyId = Nothing
  , selectedBodyIdScroll = 0
  , selectedBuilding = Nothing
  , selectedBuildingScroll = 0
  , buildingQuantity = 1
  , buildingQuantityScroll = 0
  }
