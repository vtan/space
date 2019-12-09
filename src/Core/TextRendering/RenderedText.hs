module Core.TextRendering.RenderedText
  ( RenderedText(..)
  , render )
where

import GlobalImports

import Core.Common.Rect (Rect(..))

import qualified SDL

data RenderedText = RenderedText
  { texture :: SDL.Texture
  , size :: V2 CInt
  }
  deriving (Generic)

render :: MonadIO m => SDL.Renderer -> Rect Int -> RenderedText -> m ()
render renderer (Rect pos requestedSize) RenderedText{ texture, size = textureSize } =
  let
    actualSize = min <$> fmap fromIntegral requestedSize <*> textureSize
    sourceRect = SDL.Rectangle 0 actualSize
    destinationRect = SDL.Rectangle (SDL.P (fmap fromIntegral pos)) actualSize
  in
    SDL.copy renderer texture (Just sourceRect) (Just destinationRect)

