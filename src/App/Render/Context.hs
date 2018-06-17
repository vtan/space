module App.Render.Context where

import App.Prelude

import qualified SDL

data Context = Context
  { renderer :: SDL.Renderer
  }
  deriving (Show, Generic)

fromRenderer :: SDL.Renderer -> Context
fromRenderer renderer = Context { renderer = renderer }