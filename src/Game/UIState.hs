module Game.UIState where

import App.Prelude

import qualified App.Dimension.Local as Local

import App.Common.Camera (Camera(..))
import App.Dimension.Local (Local(..))

data UIState = UIState
  { camera :: Camera (Local Double) Double
  , draggingCamera :: Bool
  }
  deriving (Generic)

initial :: V2 Double -> UIState
initial screenSize = UIState
  { camera = Camera
    { conversion = Local.iso
    , eyeFrom = V2 0 0
    , eyeTo = 0.5 *^ screenSize
    , scale = V2 200 (-200)
    }
  , draggingCamera = False
  }