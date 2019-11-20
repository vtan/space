module Game.UIState where

import GlobalImports

import qualified Game.Dimension.Local as Local
import qualified Game.ColonyWindowState as ColonyWindowState

import Game.Common.Camera (Camera(..))
import Game.Dimension.Local (Local(..))
import Game.ColonyWindowState (ColonyWindowState)

data UIState = UIState
  { camera :: Camera (Local Double) Double
  , draggingCamera :: Bool
  , colonyWindow :: ColonyWindowState
  , openWindow :: Maybe (OpenWindow)
  }
  deriving (Generic)

data OpenWindow
  = ColonyWindow
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
  , openWindow = Nothing
  }
