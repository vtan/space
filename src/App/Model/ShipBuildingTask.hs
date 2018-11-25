module App.Model.ShipBuildingTask where

import App.Prelude

import qualified App.Model.Ship as Ship

data ShipBuildingTask = ShipBuildingTask
  { typ :: Ship.Type
  , size :: Int
  , buildEffortSpent :: Int
  }
  deriving (Show, Generic)
