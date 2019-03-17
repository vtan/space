module App.Update.UIState where

import App.Prelude

import qualified App.Dimension.Local as Local
import qualified App.Model.Ship as Ship
import qualified App.Update.ListBoxState as ListBoxState

import App.Common.Camera (Camera(..))
import App.Common.Id (Id)
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
  , selectedShip :: ListBoxState (Id Ship)
  -- TODO the body lists on the colony and ship screen are both tied to this
  , selectedBody :: ListBoxState (Id Body)
  , selectedResource :: ListBoxState Resource
  , selectedInstallation :: ListBoxState Installation
  , selectedBuildingTaskIndex :: ListBoxState Int
  , selectedShipType :: ListBoxState Ship.Type
  , selectedShipSize :: Int
  , editedShipName :: Text
  , editedResourceQty :: Text
  , editedInstallationQty :: Text
  , editedPopulationQty :: Text
  , testNumber :: Int
  }
  deriving (Generic)

data Window
  = ColonyWindow
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
  , selectedBuildingTaskIndex = ListBoxState.initial
  , selectedShipType = ListBoxState.initial
  , selectedShipSize = 1
  , editedShipName = ""
  , editedResourceQty = ""
  , editedInstallationQty = ""
  , editedPopulationQty = ""
  , testNumber = 0
  }
