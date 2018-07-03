module App.Update.UIState where

import App.Prelude

import App.Body (Body)
import App.Ship (Ship)
import App.Uid (Uid)

data UIState = UIState
  { shipWindowOpen :: Bool 
  , selectedShipUid :: Maybe (Uid Ship)
  , selectedShipName :: Maybe Text
  , selectedBodyUid :: Maybe (Uid Body)
  }
  deriving (Generic)

initial :: UIState
initial = UIState
  { shipWindowOpen = False 
  , selectedShipUid = Nothing
  , selectedShipName = Nothing
  , selectedBodyUid = Nothing
  }