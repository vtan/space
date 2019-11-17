module Core.UI.Widgets where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified Core.CachedTextRenderer as CachedTextRenderer
import qualified Core.RenderedText as RenderedText
import qualified Core.UI.UI as UI

import Core.CoreContext (CoreContext(..))
import Core.UI.Theme (Theme(..))
import Core.UI.UI (UIComponent, UIContext(..))

import qualified SDL

text :: Text -> UIComponent s
text str = do
  UIContext{ cursor } <- ask
  UI.render $ \CoreContext{ renderer, cachedTextRenderer } -> do
    renderedText <- cachedTextRenderer & CachedTextRenderer.render str
    RenderedText.render renderer (fmap round cursor) renderedText
  pure mempty

button :: Text -> UIComponent s
button str = do
  UIContext{ cursor, theme = Theme{ borderColor } } <- ask
  UI.render $ \CoreContext{ renderer } -> do
    SDL.rendererDrawColor renderer $= borderColor
    SDL.drawRect renderer (Just (Rect.toSdl (fmap (round @_ @CInt) cursor)))
  text str
