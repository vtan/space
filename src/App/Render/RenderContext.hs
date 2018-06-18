module App.Render.RenderContext where

import App.Prelude

import qualified SDL
import qualified SDL.Font as SDL.TTF

data RenderContext = RenderContext
  { renderer :: SDL.Renderer
  , font :: SDL.TTF.Font
  }
  deriving (Show, Generic)

new :: SDL.Renderer -> SDL.TTF.Font -> RenderContext
new renderer font = RenderContext
  { renderer = renderer 
  , font = font
  }