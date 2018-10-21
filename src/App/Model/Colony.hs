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
  , stockpile :: HashMap Resource Int
  , installations :: HashMap Installation Int
  , buildingTask :: Maybe BuildingTask
  , shipBuildingTask :: Maybe ShipBuildingTask
  }
  deriving (Show, Generic)
