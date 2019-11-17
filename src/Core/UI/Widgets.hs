module Core.UI.Widgets where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified Core.CachedTextRenderer as CachedTextRenderer
import qualified Core.RenderedText as RenderedText
import qualified Core.UI.UI as UI

import App.Common.EventPatterns (pattern MousePressEvent)
import Core.CoreContext (CoreContext(..))
import Core.UI.Theme (Theme(..))
import Core.UI.UI (UIComponent, UIContext(..))

import qualified SDL

import Data.Semigroup (Endo(..))

text :: Text -> UIComponent s
text str = do
  UIContext{ cursor } <- ask
  UI.render $ \CoreContext{ renderer, cachedTextRenderer } -> do
    renderedText <- cachedTextRenderer & CachedTextRenderer.render str
    RenderedText.render renderer (fmap round cursor) renderedText
  pure mempty

button :: Text -> (s -> s) -> UIComponent s
button str onClick = do
  UIContext{ cursor, theme = Theme{ borderColor, highlightColor } } <- ask

  clicksInside <- UI.consumeEvents $ \case
    MousePressEvent SDL.ButtonLeft position ->
      Rect.contains cursor (fmap fromIntegral position)
    _ -> False
  let clicked = not (null clicksInside)

  UI.render $ \CoreContext{ renderer } -> do
    let rect = Just (Rect.toSdl (fmap (round @_ @CInt) cursor))
    when clicked $ do
      SDL.rendererDrawColor renderer $= highlightColor
      SDL.fillRect renderer rect
    SDL.rendererDrawColor renderer $= borderColor
    SDL.drawRect renderer rect
  text str
  pure (if clicked then Endo onClick else mempty)
