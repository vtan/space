module App.Render.RenderState where

import App.Prelude

import qualified App.Render.TextRenderer as TextRenderer

import App.Render.TextRenderer (TextRenderer)

data RenderState = RenderState
  { textRenderer :: TextRenderer }
  deriving (Generic)
  
initial :: RenderState
initial = RenderState { textRenderer = TextRenderer.new }