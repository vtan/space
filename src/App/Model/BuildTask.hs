module App.Model.BuildTask where

import App.Prelude

import App.Model.Installation (Installation)

data BuildTask = BuildTask
  { installation :: Installation
  , quantity :: Int
  , buildEffortSpent :: Int
  , installWhenDone :: Bool
  }
  deriving (Show, Generic)
