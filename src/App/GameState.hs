module App.GameState where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Rect as Rect

import App.Camera (Camera)
import App.Rect (Rect)

data GameState = GameState
  { rect :: Rect Int
  , camera :: Camera Int
  , totalTime :: Float
  , quit :: Bool
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { rect = Rect.fromMinSize 0 1
  , camera = Camera.initial
  , totalTime = 0
  , quit = False
  }
