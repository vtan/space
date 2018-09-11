module App.Update.UIState where

import App.Prelude

import App.Model.Body (Body)
import App.Model.Ship (Ship)
import App.Uid (Uid)

data UIState = UIState
  { activeWindow :: Maybe Window
  , selectedShipUid :: Maybe (Uid Ship)
  , selectedShipScrollOffset :: Int
  , selectedBodyUid :: Maybe (Uid Body)
  , selectedBodyScrollOffset :: Int
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
  , selectedShipScrollOffset = 0
  , selectedBodyUid = Nothing
  , selectedBodyScrollOffset = 0
  , editedShipName = Nothing
  }
