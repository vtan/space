module App.Model.Colony where

import App.Prelude

import App.Model.BuildingTask (BuildingTask)
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)
import App.Model.ShipBuildingTask (ShipBuildingTask)

data Colony = Colony
  { stockpile :: HashMap Resource Int
  , installations :: HashMap Installation Int
  , buildingTask :: Maybe BuildingTask
  , shipBuildingTask :: Maybe ShipBuildingTask
  }
  deriving (Show, Generic)
