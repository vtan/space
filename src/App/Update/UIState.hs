module App.Update.UIState where

import App.Prelude

import App.Model.Body (Body)
import App.Model.Ship (Ship)
import App.Uid (Uid)

data UIState = UIState
  { activeWindow :: Maybe Window
  , selectedShipUid :: Maybe (Uid Ship)
  , selectedBodyUid :: Maybe (Uid Body)
  , editedShipName :: Maybe Text
  }
  deriving (Generic)

data Window
  = ColonyWindow
  | ShipWindow
  deriving (Show, Generic, Eq)

initial :: UIState
initial = UIState
  { activeWindow = Nothing
  , selectedShipUid = Nothing
  , selectedBodyUid = Nothing
  , editedShipName = Nothing
  }