module App.Render.TextRenderer where

import App.Prelude

import qualified Data.HashSet as HashSet
import qualified SDL
import qualified SDL.Font as SDL.TTF

import App.Render.RenderContext (RenderContext)
import Control.Monad.State.Strict (get)

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

render :: (MonadState TextRenderer m, MonadReader RenderContext m, MonadIO m) => Text -> m SDL.Texture
render text = do
  TextRenderer{ cache } <- get
  texture <- case cache ^. at text of
    Just t -> pure t
    Nothing -> do
      renderer <- view #renderer
      font <- view #font
      let color = traceShowId 255
      surface <- SDL.TTF.blended font color text
      texture <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      #cache %= set (at text) (Just texture)
      pure texture
  #usedSinceLastClean %= set (contains text) True
  pure texture

clean :: (MonadState TextRenderer m, MonadIO m) => m ()
clean = do
  TextRenderer{ cache, usedSinceLastClean } <- get
  let keys = cache & fmap (const ()) & HashSet.fromMap
      unusedKeys = HashSet.difference keys usedSinceLastClean
  for_ unusedKeys $ \key -> do
    for_ (cache ^. at key) SDL.destroyTexture
    #cache %= set (at key) Nothing
  #usedSinceLastClean .= mempty