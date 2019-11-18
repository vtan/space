module Core.UI.Widgets where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified Core.CachedTextRenderer as CachedTextRenderer
import qualified Core.RenderedText as RenderedText
import qualified Core.UI.UI as UI

import App.Common.EventPatterns (pattern MousePressEvent)
import App.Common.Rect (Rect(..))
import Core.CoreContext (CoreContext(..))
import Core.UI.Theme (Theme(..))
import Core.UI.UI (UI, UIComponent, UIContext(..))

import qualified SDL

import Data.Semigroup (Endo(..))

label :: Text -> UIComponent s
label str =
  const mempty <$> text str

button :: Text -> (s -> s) -> UIComponent s
button str onClick = do
  UIContext{ cursor, theme = Theme{ borderColor, highlightColor } } <- ask
  clicked <- clickedInside cursor
  scaledCursor <- UI.scaleRect cursor
  UI.render $ ask >>= \CoreContext{ renderer } -> do
    let rect = Just scaledCursor
    when clicked $ do
      SDL.rendererDrawColor renderer $= highlightColor
      SDL.fillRect renderer rect
    SDL.rendererDrawColor renderer $= borderColor
    SDL.drawRect renderer rect
  text str
  pure (if clicked then Endo onClick else mempty)

text :: Text -> UI ()
text str = do
  UIContext{ cursor, scaleFactor } <- ask
  let scaledCursor = fmap round (scaleFactor *^ cursor)
  UI.render $ ask >>= \CoreContext{ renderer, cachedTextRenderer } -> do
    renderedText <- cachedTextRenderer & CachedTextRenderer.render str
    RenderedText.render renderer scaledCursor renderedText
  pure ()

clickedInside :: Rect Double -> UI Bool
clickedInside rect = do
  UIContext{ scaleFactor } <- ask
  fmap (not . null) . UI.consumeEvents $ \case
    MousePressEvent SDL.ButtonLeft position ->
      Rect.contains rect ((1 / scaleFactor) *^ fmap fromIntegral position)
    _ -> False