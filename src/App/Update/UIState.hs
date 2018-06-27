module App.Update.UIState where

import App.Prelude

import App.Ship (Ship)
import App.Uid (Uid)

data UIState = UIState
  { shipWindowOpen :: Bool 
  , selectedShipUid :: Maybe (Uid Ship)
  }
  deriving (Generic)

initial :: IO UIState
initial =
  pure $ UIState
    { shipWindowOpen = False 
    , selectedShipUid = Nothing
    }