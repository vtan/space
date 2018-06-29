module App.Update.UIState where

import App.Prelude

import App.Body (Body)
import App.Ship (Ship)
import App.Uid (Uid)

data UIState = UIState
  { shipWindowOpen :: Bool 
  , selectedShipUid :: Maybe (Uid Ship)
  , selectedBodyUid :: Maybe (Uid Body)
  }
  deriving (Generic)

initial :: IO UIState
initial =
  pure $ UIState
    { shipWindowOpen = False 
    , selectedShipUid = Nothing
    , selectedBodyUid = Nothing
    }