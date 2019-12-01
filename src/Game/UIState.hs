module Game.UIState where

import GlobalImports

import qualified Game.Dimension.Local as Local
import qualified Game.Colonies.ColonyWindowState as ColonyWindowState
import qualified Game.Ships.ShipWindowState as ShipWindowState

import Game.Colonies.ColonyWindowState (ColonyWindowState)
import Game.Common.Camera (Camera(..))
import Game.Dimension.Local (Local(..))
import Game.Ships.ShipWindowState (ShipWindowState)

data UIState = UIState
  { camera :: Camera (Local Double) Double
  , draggingCamera :: Bool
  , colonyWindow :: ColonyWindowState
  , shipWindow :: ShipWindowState
  , openWindow :: Maybe (OpenWindow)
  }
  deriving (Generic)

data OpenWindow
  = ColonyWindow
  | ShipWindow
  deriving (Generic, Eq)

initial :: V2 Double -> UIState
initial screenSize = UIState
  { camera = Camera
    { conversion = Local.iso
    , eyeFrom = V2 0 0
    , eyeTo = 0.5 *^ screenSize
    , scale = V2 200 (-200)
    }
  , draggingCamera = False
  , colonyWindow = ColonyWindowState.initial
  , shipWindow = ShipWindowState.initial
  , openWindow = Nothing
  }
