module App.Model.BuildTask where

import App.Prelude

import App.Model.Installation (Installation)
import Data.String (fromString)

data BuildTask = BuildTask
  { installation :: Installation
  , quantity :: Int
  , buildEffortSpent :: Int
  , installWhenDone :: Bool
  }
  deriving (Show, Generic)

print :: BuildTask -> Text
print BuildTask{..} =
  fromString $
    printf "%s (%d)" (show installation) quantity
      ++ (if installWhenDone then " (install)" else "")
