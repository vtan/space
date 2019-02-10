module App.Render.TextRenderer where

import App.Prelude

import qualified Data.HashSet as HashSet
import qualified SDL
import qualified SDL.Font as SDL.TTF

import Data.Generics.Product (HasType, typed)

data TextRenderer = TextRenderer
  { cache :: HashMap Text RenderedText
  , usedSinceLastClean :: HashSet Text
  }
  deriving (Generic)

data RenderedText = RenderedText
  { texture :: SDL.Texture
  , size :: V2 CInt
  }
  deriving (Generic)

new :: TextRenderer
new = TextRenderer
  { cache = mempty
  , usedSinceLastClean = mempty
  }

render
  :: (MonadState s m, MonadIO m, HasType TextRenderer s)
  => SDL.Renderer -> SDL.TTF.Font -> Text -> m RenderedText
render renderer font text = do
  TextRenderer{ cache } <- use typed
  renderedText <- case cache ^. at text of
    Just t -> pure t
    Nothing -> do
      let color = 255
      surface <- SDL.TTF.blended font color text
      size <- SDL.surfaceDimensions surface
      texture <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      let t = RenderedText{ texture, size }
      typed @TextRenderer . #cache %= set (at text) (Just t)
      pure t
  typed @TextRenderer . #usedSinceLastClean %= set (contains text) True
  pure renderedText

clean :: (MonadState s m, MonadIO m, HasType TextRenderer s) => m ()
clean = do
  TextRenderer{ cache, usedSinceLastClean } <- use typed
  let keys = cache & fmap (const ()) & HashSet.fromMap
      unusedKeys = HashSet.difference keys usedSinceLastClean
  for_ unusedKeys $ \key -> do
    for_ (cache ^? at key . _Just . #texture) SDL.destroyTexture
    typed @TextRenderer . #cache %= set (at key) Nothing
  typed @TextRenderer . #usedSinceLastClean .= mempty
