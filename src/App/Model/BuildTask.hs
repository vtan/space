module App.Model.BuildTask where

import GlobalImports

import App.Model.Installation (Installation)

data BuildTask = BuildTask
  { installation :: Installation
  , quantity :: Int
  , buildEffortSpent :: Int
  , installWhenDone :: Bool
  }
  deriving (Show, Generic)
