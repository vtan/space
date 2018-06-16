module App.GameState where

import App.Prelude

import qualified App.Rect as Rect

import App.Camera (Camera(..))
import App.Dims (AU, _AU)
import App.Rect (Rect)

data GameState = GameState
  { rect :: Rect (AU Double)
  , movingViewport :: Bool
  , camera :: Camera (AU Double) Double
  , totalTime :: Float
  , quit :: Bool
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { rect = Rect.fromMinSize 0 1
  , movingViewport = False
  , camera = Camera 
    { conversion = _AU
    , scale = 48
    , translate = 16
    }
  , totalTime = 0
  , quit = False
  }
