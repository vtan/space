module App.Model.BuildTask where

import App.Prelude

import qualified App.Common.Print as Print
import qualified App.Model.Installation as Installation

import App.Model.Installation (Installation)

data BuildTask = BuildTask
  { installation :: Installation
  , quantity :: Int
  , buildEffortSpent :: Int
  , installWhenDone :: Bool
  }
  deriving (Show, Generic)

print :: BuildTask -> TextBuilder
print BuildTask{..} =
  Installation.print installation <> " " <> Print.brackets (Print.int quantity)
    <> (if installWhenDone then " (install)" else " (store)")
