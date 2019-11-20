module App.Model.Colony where

import GlobalImports

import Game.Common.Id (Id)
import App.Model.Body (Body)
import App.Model.BuildTask (BuildTask)
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)
import App.Model.ShipBuildingTask (ShipBuildingTask)

data Colony = Colony
  { bodyId :: Id Body
  , population :: Int
  , isHomeworld :: Bool
  , stockpile :: HashMap Resource Double
  , installations :: HashMap Installation Int
  , buildQueue :: [BuildTask]
  , shipBuildingTask :: Maybe ShipBuildingTask
  , miningPriorities :: HashMap Resource Int
  }
  deriving (Show, Generic)
