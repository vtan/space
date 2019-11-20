module Core.RenderedText
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

render :: MonadIO m => SDL.Renderer -> Double -> Rect Double -> RenderedText -> m ()
render renderer scale (Rect pos requestedSize) RenderedText{ texture, size = textureSize } =
  let
    actualSize = min
      <$> fmap round requestedSize
      <*> fmap round (scale *^ fmap fromIntegral textureSize)
    sourceRect = SDL.Rectangle 0 textureSize
    destinationRect = SDL.Rectangle (SDL.P (fmap round pos)) actualSize
  in
    SDL.copy renderer texture (Just sourceRect) (Just destinationRect)

