module App.Model.ShipBuildingTask where

import App.Prelude

import App.Dimension.Time (Time)

data ShipBuildingTask = ShipBuildingTask
  { finishTime :: Time Int }
  deriving (Show, Generic)
