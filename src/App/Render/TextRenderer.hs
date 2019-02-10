module App.Render.TextRenderer where

import App.Prelude

import qualified Data.HashSet as HashSet
import qualified SDL
import qualified SDL.Font as SDL.TTF

import Data.Generics.Product (HasType, typed)

data TextRenderer = TextRenderer
  { cache :: HashMap Text SDL.Texture
  , usedSinceLastClean :: HashSet Text
  }
  deriving (Generic)

new :: TextRenderer
new = TextRenderer
  { cache = mempty
  , usedSinceLastClean = mempty
  }

render
  :: (MonadState s m, MonadIO m, HasType TextRenderer s)
  => SDL.Renderer -> SDL.TTF.Font -> Text -> m SDL.Texture
render renderer font text = do
  TextRenderer{ cache } <- use typed
  texture <- case cache ^. at text of
    Just t -> pure t
    Nothing -> do
      let color = 255
      surface <- SDL.TTF.blended font color text
      texture <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      typed @TextRenderer . #cache %= set (at text) (Just texture)
      pure texture
  typed @TextRenderer . #usedSinceLastClean %= set (contains text) True
  pure texture

clean :: (MonadState s m, MonadIO m, HasType TextRenderer s) => m ()
clean = do
  TextRenderer{ cache, usedSinceLastClean } <- use typed
  let keys = cache & fmap (const ()) & HashSet.fromMap
      unusedKeys = HashSet.difference keys usedSinceLastClean
  for_ unusedKeys $ \key -> do
    for_ (cache ^. at key) SDL.destroyTexture
    typed @TextRenderer . #cache %= set (at key) Nothing
  typed @TextRenderer . #usedSinceLastClean .= mempty
