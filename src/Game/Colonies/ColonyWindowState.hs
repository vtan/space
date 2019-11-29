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
  , bodyScrollOffset :: Double
  , selectedBuilding :: Maybe (Building)
  , buildingQuantity :: Int
  }
  deriving (Generic)

initial :: ColonyWindowState
initial = ColonyWindowState
  { selectedBodyId = Nothing
  , bodyScrollOffset = 0
  , selectedBuilding = Nothing
  , buildingQuantity = 1
  }
