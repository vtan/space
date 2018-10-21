module App.Model.Colony where

import App.Prelude

import App.Common.Uid (Uid)
import App.Model.Body (Body)
import App.Model.BuildingTask (BuildingTask)
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)
import App.Model.ShipBuildingTask (ShipBuildingTask)

data Colony = Colony
  { bodyUid :: Uid Body
  , population :: Int
  , isHomeworld :: Bool
  , stockpile :: HashMap Resource Double
  , installations :: HashMap Installation Double
  , buildingTask :: Maybe BuildingTask
  , shipBuildingTask :: Maybe ShipBuildingTask
  , miningPriorities :: HashMap Resource Int
  }
  deriving (Show, Generic)
