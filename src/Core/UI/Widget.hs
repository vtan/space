module Core.UI.Widget
  ( label, label', button, toggleLabel
  , list, window
  )
where

import GlobalImports

import qualified Core.Common.Rect as Rect
import qualified Core.TextRendering.CachedTextRenderer as CachedTextRenderer
import qualified Core.TextRendering.RenderedText as RenderedText
import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI

import Core.CoreContext (CoreContext(..))
import Core.Common.EventPatterns (pattern MousePressEvent)
import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained)
import Core.UI.Theme (Theme(..))
import Core.UI.UI (UI, UIComponent, UIContext(..))

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified SDL

import Data.Semigroup (Endo(..))

label :: TextBuilder -> UIComponent s
label =
  label' . LazyText.toStrict . TextBuilder.toLazyText

label' :: Text -> UIComponent s
label' str =
  const mempty <$> text' str

button :: TextBuilder -> (s -> s) -> UIComponent s
button str onClick = do
  UIContext{ cursor, theme = Theme{ borderColor, widgetBackgroundColor, highlightColor } } <- ask
  clicked <- clickedInside cursor
  scaledCursor <- UI.scaleRect cursor
  UI.render $ ask >>= \CoreContext{ renderer } -> do
    let
      rect = Just scaledCursor
      backgroundColor = if clicked then highlightColor else widgetBackgroundColor
    SDL.rendererDrawColor renderer $= backgroundColor
    SDL.fillRect renderer rect
    SDL.rendererDrawColor renderer $= borderColor
    SDL.drawRect renderer rect
  text str
  pure (if clicked then Endo onClick else mempty)

toggleLabel :: TextBuilder -> Bool -> (s -> s) -> UIComponent s
toggleLabel str isActive onClick = do
  UIContext{ cursor, theme = Theme{ widgetBackgroundColor, selectionBackgroundColor } } <- ask
  clicked <- clickedInside cursor
  scaledCursor <- UI.scaleRect cursor
  UI.render $ ask >>= \CoreContext{ renderer } -> do
    let
      rect = Just scaledCursor
      color = if isActive then selectionBackgroundColor else widgetBackgroundColor
    SDL.rendererDrawColor renderer $= color
    SDL.fillRect renderer rect
  text str
  pure (if clicked then Endo onClick else mempty)

list
  :: forall a i s. Eq i
  => (a -> i)
  -> (a -> TextBuilder)
  -> [a]
  -> Maybe i
  -> (Maybe i -> s -> s)
  -> UIComponent s
list toIndex toText items selectedIndex onSelect =
  local (set #layoutGap 0) $
    Layout.vertical (map toChild items)
      where
        toChild :: a -> Constrained (UIComponent s)
        toChild item =
          let
            index = toIndex item
            isActive = elem index selectedIndex
            onClick = onSelect (Just index)
          in
            Layout.DefaultSized (toggleLabel (toText item) isActive onClick)

window :: Text -> UIComponent s -> UIComponent s
window title child = do
  let decorationHeight = 20
  UIContext{ cursor, theme = Theme{ backgroundColor, windowDecorationColor } } <- ask
  do
    decoration <- UI.scaleRect (set (#wh . _y) decorationHeight cursor)
    body <- UI.scaleRect $
      cursor
        & over (#xy . _y) (+ decorationHeight)
        & over (#wh . _y) (subtract decorationHeight)
    UI.render $ ask >>= \CoreContext{ renderer } -> do
      SDL.rendererDrawColor renderer $= windowDecorationColor
      SDL.fillRect renderer (Just decoration)
      SDL.rendererDrawColor renderer $= backgroundColor
      SDL.fillRect renderer (Just body)
  result <- Layout.vertical
    [ Layout.Sized 20 (label' title)
    , Layout.Stretched child
    ]
  _ <- clickedInside cursor
  pure result

text :: TextBuilder -> UI ()
text =
  text' . LazyText.toStrict . TextBuilder.toLazyText

text' :: Text -> UI ()
text' str = do
  UIContext{ cursor, scaleFactor } <- ask
  let scaledCursor = scaleFactor *^ cursor
  UI.render $ ask >>= \CoreContext{ renderer, cachedTextRenderer } -> do
    renderedText <- cachedTextRenderer & CachedTextRenderer.render str
    RenderedText.render renderer scaleFactor scaledCursor renderedText
  pure ()

clickedInside :: Rect Double -> UI Bool
clickedInside rect = do
  UIContext{ scaleFactor } <- ask
  fmap (not . null) . UI.consumeEvents $ \case
    MousePressEvent SDL.ButtonLeft position ->
      Rect.contains rect ((1 / scaleFactor) *^ fmap fromIntegral position)
    _ -> False
