module App.GameState where

import App.Prelude

import qualified App.Rect as Rect

import App.Camera (Camera(..))
import App.Rect (Rect)

data GameState = GameState
  { rect :: Rect Int
  , camera :: Camera Int Int
  , totalTime :: Float
  , quit :: Bool
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { rect = Rect.fromMinSize 0 1
  , camera = Camera 
    { conversion = id
    , scale = 48
    , translate = 16
    }
  , totalTime = 0
  , quit = False
  }
