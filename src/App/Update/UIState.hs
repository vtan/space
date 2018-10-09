module App.Update.UIState where

import App.Prelude

import qualified App.Update.ListBoxState as ListBoxState

import App.Model.Body (Body)
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)
import App.Model.Ship (Ship)
import App.Uid (Uid)
import App.Update.ListBoxState (ListBoxState)

data UIState = UIState
  { activeWindow :: Maybe Window
  -- TODO put these into window-specific substates?
  , selectedShip :: ListBoxState (Uid Ship)
  -- TODO the body lists on the colony and ship screen are both tied to this
  , selectedBody :: ListBoxState (Uid Body)
  , selectedResource :: ListBoxState Resource
  , selectedInstallation :: ListBoxState Installation
  , editedShipName :: Text
  , editedResourceQty :: Text
  , editedInstallationQty :: Text
  , editedPopulationQty :: Text
  }
  deriving (Generic)

data Window
  = ColonyWindow
  | ShipWindow
  deriving (Show, Generic, Eq)

initial :: UIState
initial = UIState
  { activeWindow = Nothing
  , selectedShip = ListBoxState.initial
  , selectedBody = ListBoxState.initial
  , selectedResource = ListBoxState.initial
  , selectedInstallation = ListBoxState.initial
  , editedShipName = ""
  , editedResourceQty = ""
  , editedInstallationQty = ""
  , editedPopulationQty = ""
  }
