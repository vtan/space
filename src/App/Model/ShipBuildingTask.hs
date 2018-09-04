module App.Model.ShipBuildingTask where

import App.Prelude

data ShipBuildingTask = ShipBuildingTask
  { finishTime :: Int }
  deriving (Show, Generic)