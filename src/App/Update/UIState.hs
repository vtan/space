module App.Update.UIState where

import App.Prelude

data UIState = UIState
  { shipWindowOpen :: Bool }
  deriving (Show, Generic)

initial :: UIState
initial = UIState
  { shipWindowOpen = False }