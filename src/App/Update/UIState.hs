module App.Update.UIState where

import App.Prelude

import App.Body (Body)
import App.Ship (Ship)
import App.Uid (Uid)
import App.Update.WidgetState (TextBoxState)

data UIState = UIState
  { shipWindowOpen :: Bool 
  , selectedShipUid :: Maybe (Uid Ship)
  , selectedShipName :: Maybe TextBoxState
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