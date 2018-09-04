module App.Model.Colony where

import App.Prelude

import App.Model.BodyMinerals (Mineral)
import App.Model.BuildingTask (BuildingTask)

data Colony = Colony
  { stockpile :: HashMap Mineral Double 
  , mines :: HashMap Mineral Int
  , buildingTask :: Maybe BuildingTask
  }
  deriving (Show, Generic)