module Core.CachedTextRenderer where

import GlobalImports

import qualified Core.TextRenderer as TextRenderer

import Core.RenderedText (RenderedText(..))
import Core.TextRenderer (TextRenderer(..))

import qualified Data.HashMap.Strict as HashMap
import qualified SDL

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef)
import Data.StateVar (get, ($~), ($=))

data CachedTextRenderer = CachedTextRenderer
  { textRenderer :: TextRenderer
  , cache :: IORef (HashMap Text RenderedText)
  , usedSinceLastClean :: IORef (HashSet Text)
  }
  deriving (Generic)

new :: MonadIO m => TextRenderer -> m CachedTextRenderer
new textRenderer = do
  cache <- liftIO (newIORef mempty)
  usedSinceLastClean <- liftIO (newIORef mempty)
  pure CachedTextRenderer{ textRenderer, cache, usedSinceLastClean }

render :: MonadIO m => Text -> CachedTextRenderer -> m RenderedText
render text CachedTextRenderer{ textRenderer, cache, usedSinceLastClean } = do
  currentCache <- get cache
  renderedText <- case view (at text) currentCache of
    Just cached -> pure cached
    Nothing -> do
      rendered <- textRenderer & TextRenderer.render text
      cache $~ set (at text) (Just rendered)
      pure rendered
  usedSinceLastClean $~ set (contains text) True
  pure renderedText

clean :: MonadIO m => CachedTextRenderer -> m ()
clean CachedTextRenderer{ cache, usedSinceLastClean } = do
  textsToKeep <- get usedSinceLastClean
  usedSinceLastClean $= mempty
  allCached <- get cache
  cache $~ HashMap.filterWithKey (\text _ ->
      view (contains text) textsToKeep
    )
  ifor_ allCached $ \text RenderedText{ texture } ->
    if view (contains text) textsToKeep
    then pure ()
    else SDL.destroyTexture texture
