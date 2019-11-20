module App.Model.ShipBuildingTask where

import GlobalImports

import qualified App.Model.Ship as Ship

data ShipBuildingTask = ShipBuildingTask
  { typ :: Ship.Type
  , size :: Int
  , buildEffortSpent :: Int
  }
  deriving (Show, Generic)
