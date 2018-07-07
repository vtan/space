module App.Update.UIState where

import App.Prelude

import App.Model.Body (Body)
import App.Model.Ship (Ship)
import App.Uid (Uid)

data UIState = UIState
  { shipWindowOpen :: Bool 
  , selectedShipUid :: Maybe (Uid Ship)
  , selectedBodyUid :: Maybe (Uid Body)
  , editedShipName :: Maybe Text
  }
  deriving (Generic)

initial :: UIState
initial = UIState
  { shipWindowOpen = False 
  , selectedShipUid = Nothing
  , selectedBodyUid = Nothing
  , editedShipName = Nothing
  }