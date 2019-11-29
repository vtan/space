module Core.UI.Widget
  ( label, label', button, toggleLabel
  , list, scrollList, window
  )
where

import GlobalImports

import qualified Core.Common.Rect as Rect
import qualified Core.TextRendering.CachedTextRenderer as CachedTextRenderer
import qualified Core.TextRendering.RenderedText as RenderedText
import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI

import Core.CoreContext (CoreContext(..))
import Core.Common.EventPatterns (pattern MousePressEvent, pattern MouseWheelEvent)
import Core.Common.Rect (Rect(..))
import Core.Common.Util (clamp)
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

scrollList
  :: forall a i s. Eq i
  => (a -> i)
  -> (a -> TextBuilder)
  -> [a]
  -> Maybe i
  -> Double
  -> (i -> s -> s)
  -> (Double -> s -> s)
  -> UIComponent s
scrollList toIndex toText items selectedIndex scrollPosition onSelect onScroll = do
  UIContext
    { cursor = cursor@(Rect (V2 _ cursorTop) (V2 _ cursorHeight))
    , scaleFactor
    , scaledMousePosition
    , defaultSize = V2 _ itemHeight
    , theme = Theme{ borderColor, widgetBackgroundColor, highlightColor }
    } <- ask
  scaledCursor <- UI.scaleRect cursor

  let baseItemCursor = cursor & set (#wh . _y) itemHeight
  scrollOffsetChange <-
    if Rect.contains cursor scaledMousePosition
    then
      UI.consumeEvents' \case
          MouseWheelEvent direction -> Just (-32 * fromIntegral direction)
          _ -> Nothing
        & fmap sum
        & fmap \change ->
            if change == 0
            then Nothing
            else
              let maxOffset = max 0 (itemHeight * fromIntegral (length items) - cursorHeight)
              in Just (clamp 0 (scrollPosition + change) maxOffset)
    else
      pure Nothing
  clickedItem <- listToMaybe <$> UI.consumeEvents' \case
    MousePressEvent SDL.ButtonLeft scaledPosition ->
      let
        position@(V2 _ y) = (1 / scaleFactor) *^ fmap fromIntegral scaledPosition
        clickedIndex = floor ((y - cursorTop + scrollPosition) / itemHeight)
      in
        if Rect.contains cursor position
        then preview (ix clickedIndex) items
        else Nothing
    _ -> Nothing

  UI.renderWithClipRect scaledCursor $ ask >>= \CoreContext{ renderer } -> do
    let listRect = Just scaledCursor
    SDL.rendererDrawColor renderer $= widgetBackgroundColor
    SDL.fillRect renderer listRect
    SDL.rendererDrawColor renderer $= borderColor
    SDL.drawRect renderer listRect

    -- TODO do not render items outside viewport
    ifor_ items \index item -> do
      let
        itemCursor = baseItemCursor & over (#xy . _y) \y ->
          itemHeight * fromIntegral index + y - scrollPosition
        isActive = elem (toIndex item) selectedIndex
        rect = scaleFactor *^ itemCursor
      when isActive do
        SDL.rendererDrawColor renderer $= highlightColor
        SDL.fillRect renderer (Just (Rect.toSdl (fmap (round @Double @CInt) rect)))
      renderText scaleFactor rect (LazyText.toStrict . TextBuilder.toLazyText . toText $ item)

  pure $ fold @[]
    [ case scrollOffsetChange of
        Just offset -> Endo (onScroll offset)
        Nothing -> mempty
    , case clickedItem of
        Just item -> Endo (onSelect (toIndex item))
        Nothing -> mempty
    ]

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
  UI.render $ renderText scaleFactor scaledCursor str

renderText :: Double -> Rect Double -> Text -> ReaderT CoreContext IO ()
renderText scaleFactor cursor str =
  ask >>= \CoreContext{ renderer, cachedTextRenderer } -> do
    renderedText <- cachedTextRenderer & CachedTextRenderer.render str
    RenderedText.render renderer scaleFactor cursor renderedText

clickedInside :: Rect Double -> UI Bool
clickedInside rect = do
  UIContext{ scaleFactor } <- ask
  fmap (not . null) . UI.consumeEvents $ \case
    MousePressEvent SDL.ButtonLeft position ->
      Rect.contains rect ((1 / scaleFactor) *^ fmap fromIntegral position)
    _ -> False
