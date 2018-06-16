module App.Render
  ( render )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Rect as Rect
import qualified SDL as SDL

import App.GameState (GameState(..))
import SDL (($=))

render :: SDL.Renderer -> GameState -> IO ()
render renderer GameState{ rect, camera } = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 0 0 255 255
  SDL.drawRect renderer . Just . Rect.toSdl . Camera.rectToScreen camera $ rect
  SDL.present renderer