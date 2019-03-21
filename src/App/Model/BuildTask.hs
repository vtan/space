module App.Model.BuildTask where

import App.Prelude

import qualified App.Common.Display as Display
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
  Installation.print installation <> " " <> Display.brackets (Display.int quantity)
    <> (if installWhenDone then " (install)" else " (store)")
