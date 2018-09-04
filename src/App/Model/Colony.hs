module App.Model.Colony where

import App.Prelude

import App.Model.BodyMinerals (Mineral)
import App.Model.BuildingTask (BuildingTask)
import App.Model.ShipBuildingTask (ShipBuildingTask)

data Colony = Colony
  { stockpile :: HashMap Mineral Double 
  , mines :: HashMap Mineral Int
  , buildingTask :: Maybe BuildingTask
  , shipBuildingTask :: Maybe ShipBuildingTask
  }
  deriving (Show, Generic)