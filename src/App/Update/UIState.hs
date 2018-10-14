module App.Update.UIState where

import App.Prelude

import qualified App.Dimension.Local as Local
import qualified App.Update.ListBoxState as ListBoxState

import App.Common.Camera (Camera(..))
import App.Common.Uid (Uid)
import App.Dimension.Local (Local)
import App.Model.Body (Body)
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)
import App.Model.Ship (Ship)
import App.Update.ListBoxState (ListBoxState)

data UIState = UIState
  { camera :: Camera (Local Double) Double
  , activeWindow :: Maybe Window
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
  | ProductionWindow
  deriving (Show, Generic, Eq)

initial :: UIState
initial = UIState
  { camera = Camera
    { conversion = Local.iso
    , eyeFrom = V2 0 0
    , eyeTo = 0.5 *^ V2 1728 972
    , scale = V2 200 (-200)
    }
  , activeWindow = Nothing
  , selectedShip = ListBoxState.initial
  , selectedBody = ListBoxState.initial
  , selectedResource = ListBoxState.initial
  , selectedInstallation = ListBoxState.initial
  , editedShipName = ""
  , editedResourceQty = ""
  , editedInstallationQty = ""
  , editedPopulationQty = ""
  }
