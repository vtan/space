module App.UIBuilder.Widget where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified App.Render.Render as Render
import qualified App.UIBuilder.UIBuilder as UI
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified SDL

import App.Common.EventPatterns
import App.Common.Rect (Rect(..))
import App.Common.Util (clamp)
import App.UIBuilder.ListBoxState (ListBoxState)
import App.UIBuilder.UIBuilder (MonadUI, UIBuilderContext(..), UIGroup(..), UIBuilderState(..))
import App.UIBuilder.Unscaled (Unscaled(..))
import Control.Lens (Lens')
import Data.Generics.Product (typed)

label :: MonadUI r s m => TextBuilder -> m ()
label =
  label' . LazyText.toStrict . TextBuilder.toLazyText

label' :: MonadUI r s m => Text -> m ()
label' text =
  UI.placeWidget $ do
    bounds <- UI.nextWidgetScaled
    UI.render (Render.text bounds text)

button :: MonadUI r s m => Text -> m Bool
button text =
  UI.placeWidget $ do
    bounds <- UI.nextWidgetScaled
    Any clicked <- UI.consumeEvents $ \case
      MousePressEvent SDL.ButtonLeft pos ->
        Any (Rect.contains bounds (fromIntegral <$> pos))
      _ -> mempty
    UI.render $ do
      r <- view #renderer
      let color = if clicked then highlight else shade3
      SDL.rendererDrawColor r $= color
      SDL.fillRect r (Just $ Rect.toSdl bounds)
      Render.text bounds text
    pure clicked

textBox :: MonadUI r s m => Text -> Lens' s Text -> m Text
textBox name state =
  UI.placeWidget $ do
    UIBuilderState{ focusedWidgetName } <- use typed
    bounds <- UI.nextWidgetScaled
    focused <- do
      Any clicked <- UI.consumeEvents $ \case
        MousePressEvent SDL.ButtonLeft pos ->
          Any (Rect.contains bounds (fromIntegral <$> pos))
        _ -> mempty
      if clicked
      then True <$ (typed @UIBuilderState . #focusedWidgetName .= Just name)
      else pure (elem name focusedWidgetName)
    text <- use state
    text' <-
      if focused
      then do
        textMods <- UI.consumeEvents $ \case
          TextInputEvent newText -> [(<> newText)]
          KeyPressEvent SDL.ScancodeBackspace -> [Text.dropEnd 1]
          e | isUnicodeKeyEvent e -> [id] -- consume key events for which we also had a text input event
          _ -> []
        let new = foldl' @[] (&) text textMods
        when (textMods & not . null) $
          state .= new
        pure new
      else pure text
    UI.render $ do
      r <- view #renderer
      SDL.rendererDrawColor r $= shade2
      SDL.fillRect r (Just $ Rect.toSdl bounds)
      Render.text bounds text'
      SDL.rendererDrawColor r $= if focused then highlight else shade1
      SDL.drawRect r (Just $ Rect.toSdl bounds)
    pure text'

data ListBox a i = ListBox
  { itemHeight :: Unscaled Int
  , scrollBarSize :: V2 (Unscaled Int)
  , toIx :: a -> i
  , toText :: a -> Text
  }
  deriving (Generic)

listBox
  :: (MonadUI r s m, Eq i)
  => ListBox a i
  -> Lens' s (ListBoxState i)
  -> [a]
  -> m (Maybe a, Bool)
listBox ListBox{ itemHeight, scrollBarSize, toIx, toText } state items =
  UI.placeWidget $ do
    UIBuilderState{ groups = UIGroup{ nextWidget } :| _ } <- use typed
    bounds <- UI.nextWidgetScaled
    UIBuilderContext{ mousePosition } <- view typed
    Sum scrollDiff <-
      if Rect.contains bounds mousePosition
      then
        UI.consumeEvents $ \case
          MouseWheelEvent diff -> Sum (fromIntegral diff * (-2) * itemHeight)
          _ -> mempty
      else
        pure mempty
    (fmap getFirst -> clickedPos) <-
      UI.consumeEvents $ \case
        MousePressEvent SDL.ButtonLeft pos ->
          if Rect.contains bounds (fromIntegral <$> pos)
          then Just . First . fmap fromIntegral $ pos
          else mempty
        _ -> mempty
    let hiddenHeight = (length items *^ itemHeight) - (nextWidget ^. #wh . _y)
    scrollOffset <- case scrollDiff of
      _ | hiddenHeight < 0 -> pure 0
      0 -> use (state . #scrollOffset)
      _ -> do
        current <- use (state . #scrollOffset)
        let new = clamp 0 (current + scrollDiff) hiddenHeight
        state . #scrollOffset .= new
        pure new
    -- TODO this works as long as `itemHeight` and `scrollOffset` are divisors of the box height
    scaler <- UI.scaler
    let scrolledPastItemNo = getUnscaled (scrollOffset `div` itemHeight)
        shownItemNo = getUnscaled ((nextWidget ^. #wh . _y) `div` itemHeight)
        shownItems = items & drop scrolledPastItemNo & take shownItemNo
        itemHeightScaled = scaler itemHeight
        clickedRow = clickedPos <&> \pos ->
          ((pos - (bounds ^. #xy)) ^. _y) `div` itemHeightScaled + scrolledPastItemNo
        clickedItem = clickedRow >>= \i -> items ^? ix i
        clickedIx = toIx <$> clickedItem
    selectedIx <- state . #selectedIndex <<%= (clickedIx <|>)
    let (selectedPos, selectedItem) = munzip $ selectedIx >>= \i ->
          items & ifind (\_ item -> toIx item == i)
        -- TODO ^ could be better?
        selectedRow = selectedPos
          & fmap (subtract scrolledPastItemNo)
          & mfilter (\pos -> pos >= 0 && pos < shownItemNo)
        texts = map toText shownItems
        scrollRatio :: Maybe Double
          | hiddenHeight <= 0 = Nothing
          | otherwise = Just $ (fromIntegral scrollOffset / fromIntegral hiddenHeight)
        scrollBarScaled = scaler <$> scrollBarSize
    UI.render $ do
      r <- view #renderer
      SDL.rendererDrawColor r $= shade1
      SDL.fillRect r (Just $ Rect.toSdl bounds)
      for_ selectedRow $ \row -> do
        let rect = bounds
              & #xy . _y +~ row * itemHeightScaled
              & #wh . _y .~ itemHeightScaled
        SDL.rendererDrawColor r $= highlight
        SDL.fillRect r (Just $ Rect.toSdl rect)
      ifor_ texts $ \row text ->
        let rect = bounds & #xy . _y +~ row * itemHeightScaled
        in Render.text rect text
      for_ scrollRatio $ \ ratio -> do
        let x = (bounds ^. #xy . _x) + (bounds ^. #wh . _x) - (scrollBarScaled ^. _x)
            y = (bounds ^. #xy . _y) + floor (ratio * fromIntegral ((bounds ^. #wh . _y) - (scrollBarScaled ^. _y)))
            rect = Rect.fromMinSize (V2 x y) (V2 4 8)
        SDL.rendererDrawColor r $= shade3
        SDL.fillRect r (Just $ Rect.toSdl rect)
    pure (selectedItem, has _Just clickedIx)

data Window = Window
  { titleHeight :: Unscaled Int
  , title :: Text
  }
  deriving (Show, Generic)

window :: MonadUI r s m => Window -> m a -> m a
window Window{ titleHeight, title } body =
  UI.placeWidget $ do
    scaler <- UI.scaler
    UIBuilderState{ groups = UIGroup{ nextWidget, padding } :| _ } <- use typed
    let
      titleHeightScaled = scaler titleHeight
      bounds = scaler <$> nextWidget
    UI.render $ do
      let titleBounds = bounds & #wh . _y .~ titleHeightScaled
          restBounds = bounds
            & #xy . _y +~ titleHeightScaled
            & #wh . _y -~ titleHeightScaled
      r <- view #renderer
      SDL.rendererDrawColor r $= shade2
      SDL.fillRect r (Just $ Rect.toSdl titleBounds)
      SDL.rendererDrawColor r $= shade0
      SDL.fillRect r (Just $ Rect.toSdl restBounds)
      Render.text bounds title
    let Rect pos size = nextWidget
    UI.positioned (pos + padding + V2 0 titleHeight) $
      UI.sized (size - padding - V2 0 titleHeight) $
        body

shade0, shade1, shade2, shade3, highlight :: Num a => V4 a
shade0 = V4 23 23 23 255
shade1 = V4 31 31 31 255
shade2 = V4 63 63 63 255
shade3 = V4 91 91 91 255
highlight = V4 31 171 171 255
