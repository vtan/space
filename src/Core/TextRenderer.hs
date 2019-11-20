module Core.TextRenderer
  ( TextRenderer(..)
  , render
  )
where

import GlobalImports

import Core.RenderedText (RenderedText(..))

import qualified SDL
import qualified SDL.Font as SDL.TTF

data TextRenderer = TextRenderer
  { renderer :: SDL.Renderer
  , font :: SDL.TTF.Font
  }
  deriving (Generic)

render :: MonadIO m => Text -> TextRenderer -> m RenderedText
render text TextRenderer{ renderer, font } = do
  let color = 255
  surface <- SDL.TTF.blended font color text
  size <- SDL.surfaceDimensions surface
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure RenderedText{ texture, size }
