module App.Update.Widget where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified App.Update.Updating as Updating
import qualified Data.Text as Text
import qualified SDL

import App.Common.Rect (Rect)
import App.Common.Util (clamp)
import App.Render.Rendering (Rendering)
import App.Update.Events
import App.Update.ListBoxState (ListBoxState)
import App.Update.SlotId (SlotId)
import App.Update.UIState (UIState)
import App.Update.Updating (Updating)
import Control.Lens (Lens')
import Control.Monad (mfilter)
import Control.Monad.Zip (munzip)

label :: V2 Int -> Text -> Updating ()
label pos text = Updating.render $ Rendering.text pos text

labels :: V2 Int -> Int -> [Text] -> Updating ()
labels firstPos verticalSpacing texts =
  Updating.render $
    ifor_ texts $ \i text ->
      let pos = firstPos & _y +~ i * verticalSpacing
      in Rendering.text pos text

button :: Rect Int -> Text -> Updating Bool
button bounds text = do
  clicked <- Updating.consumeEvents (\case
      MousePressEvent SDL.ButtonLeft pos | Rect.contains bounds (fromIntegral <$> pos) -> Just ()
      _ -> Nothing
    ) <&> (not . null)
  Updating.render $ do
    r <- view #renderer
    let color = if clicked then highlight else shade3
    SDL.rendererDrawColor r $= color
    SDL.fillRect r (Just $ Rect.toSdl bounds)
    Rendering.text (bounds ^. #xy) text
  pure clicked

listBox :: Eq i
  => Rect Int -> Int
  -> (a -> i) -> (a -> Text)
  -> Lens' UIState (ListBoxState i)
  -> [a] -> Updating (Maybe a, Maybe a)
listBox bounds verticalSpacing toIx toText state items = do
  let hiddenHeight = length items * verticalSpacing - bounds ^. #wh . _y
  mouseInside <- use #mousePosition <&> Rect.contains bounds
  scrollDiff <- if
    | mouseInside -> Updating.consumeEvents (\case
          MouseWheelEvent diff -> Just (fromIntegral diff * (-2) * verticalSpacing)
          _ -> Nothing
        ) <&> sum
    | otherwise -> pure 0
  clickedPos <- Updating.consumeEvents (\case
      MousePressEvent SDL.ButtonLeft pos | Rect.contains bounds (fromIntegral <$> pos) ->
        Just (fromIntegral <$> pos)
      _ -> Nothing
    ) <&> listToMaybe
  scrollOffset <- case scrollDiff of
    _ | hiddenHeight < 0 -> pure 0
    0 -> use (#ui . state . #scrollOffset)
    _ -> do
      current <- use (#ui . state . #scrollOffset)
      let new = clamp 0 (current + scrollDiff) hiddenHeight
      #ui . state . #scrollOffset .= new
      pure new
  -- TODO this works as long as `verticalSpacing` and `scrollOffset` are divisors of the box height
  let scrolledPastItemNo = scrollOffset `div` verticalSpacing
      shownItemNo = (bounds ^. #wh . _y) `div` verticalSpacing
      shownItems = items & drop scrolledPastItemNo & take shownItemNo
      clickedRow = clickedPos <&> \pos ->
        ((pos - (bounds ^. #xy)) ^. _y) `div` verticalSpacing + scrolledPastItemNo
      clickedItem = clickedRow >>= \i -> items ^? ix i
      clickedIx = toIx <$> clickedItem
  selectedIx <- #ui . state . #selectedIndex <<%= (clickedIx <|>)
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
  Updating.render $ do
    r <- view #renderer
    SDL.rendererDrawColor r $= shade1
    SDL.fillRect r (Just $ Rect.toSdl bounds)
    for_ selectedRow $ \row -> do
      let rect = bounds
            & #xy . _y +~ row * verticalSpacing
            & #wh . _y .~ verticalSpacing
      SDL.rendererDrawColor r $= highlight
      SDL.fillRect r (Just $ Rect.toSdl rect)
    ifor_ texts $ \row text ->
      let pos = (bounds ^. #xy) & _y +~ row * verticalSpacing
      in Rendering.text pos text
    for_ scrollRatio $ \ ratio -> do
      let x = bounds ^. #xy . _x + bounds ^. #wh . _x - 4
          y = bounds ^. #xy . _y + floor (ratio * fromIntegral (bounds ^. #wh . _y - 8))
          rect = Rect.fromMinSize (V2 x y) (V2 4 8)
      SDL.rendererDrawColor r $= shade3
      SDL.fillRect r (Just $ Rect.toSdl rect)
  pure (selectedItem, clickedItem)

closedDropdown :: Eq i
  => Rect Int -> Int -> Int
  -> (a -> i) -> (a -> Text)
  -> Lens' UIState (ListBoxState i)
  -> [a] -> Updating (Maybe a)
closedDropdown bounds verticalSpacing openHeight toIx toText state items = do
  clickedOpen <- Updating.consumeEvents (\case
      MousePressEvent SDL.ButtonLeft pos
        | Rect.contains bounds (fromIntegral <$> pos) -> Just ()
      _ -> Nothing
    ) <&> (not . null)
  when clickedOpen $ do
    let this = () <$ openDropdown bounds verticalSpacing openHeight toIx toText state items
    #activeDropdown .= Just this
  selectedIx <- use (#ui . state . #selectedIndex)
  let selectedItem = selectedIx >>= \i -> items & find (toIx >>> (== i))
      text = selectedItem & fmap toText & fromMaybe ""
  Updating.render $ dropdownRendering bounds text
  pure selectedItem

openDropdown :: Eq i
  => Rect Int -> Int -> Int
  -> (a -> i) -> (a -> Text)
  -> Lens' UIState (ListBoxState i)
  -> [a] -> Updating (Maybe a)
openDropdown bounds verticalSpacing openHeight toIx toText state items = do
  selectedIx <- use (#ui . state . #selectedIndex)
  let selectedItem = selectedIx >>= \i -> items & find (toIx >>> (== i))
      text = selectedItem & fmap toText & fromMaybe ""
  (selectedItem', clickedItem) <- do
    let openListBounds = bounds
          & #xy . _y +~ verticalSpacing
          & #wh . _y .~ openHeight
    listBox openListBounds verticalSpacing toIx toText state items
  clickedOutside <- Updating.consumeEvents (\case
      MousePressEvent{} -> Just ()
      _ -> Nothing
    ) <&> (not . null)
  _ <- Updating.consumeEvents $ const (Just ())
  when (clickedOutside || has _Just clickedItem) $
    #activeDropdown .= Nothing
  Updating.render $ dropdownRendering bounds text
  pure selectedItem'

dropdownRendering :: Rect Int -> Text -> Rendering ()
dropdownRendering bounds text = do
  r <- view #renderer
  SDL.rendererDrawColor r $= shade3
  SDL.fillRect r (Just $ Rect.toSdl bounds)
  Rendering.text (bounds ^. #xy) text
  let arrowPos = bounds ^. #xy & _x +~ (bounds ^. #wh . _x) - 20
  Rendering.text arrowPos "▼"

window :: Rect Int -> Int -> Text -> Updating ()
window bounds titleHeight title =
  Updating.render $ do
    let titleBounds = bounds & #wh . _y .~ titleHeight
        restBounds = bounds
          & #xy . _y +~ titleHeight
          & #wh . _y -~ titleHeight
    r <- view #renderer
    SDL.rendererDrawColor r $= shade2
    SDL.fillRect r (Just $ Rect.toSdl titleBounds)
    SDL.rendererDrawColor r $= shade0
    SDL.fillRect r (Just $ Rect.toSdl restBounds)
    Rendering.text (bounds ^. #xy) title

textBox :: SlotId -> Rect Int -> Lens' UIState Text -> Updating Text
textBox slotId bounds state = do
  focused <- do
    clicked <- Updating.consumeEvents (\case
        MousePressEvent SDL.ButtonLeft pos | Rect.contains bounds (fromIntegral <$> pos) -> Just ()
        _ -> Nothing
      ) <&> (not . null)
    if clicked
    then True <$ (#focusedWidget .= Just slotId)
    else elem slotId <$> use #focusedWidget
  text <- use (#ui . state)
  text' <- if focused
    then do
      textMods <- Updating.consumeEvents $ \case
        TextInputEvent newText -> Just (<> newText)
        KeyPressEvent SDL.ScancodeBackspace -> Just (Text.dropEnd 1)
        e | isUnicodeKeyEvent e -> Just id -- consume key events for which we also had a text input event
        _ -> Nothing
      let new = foldl' (&) text textMods
      when (textMods & not . null) $
        #ui . state .= new
      pure new
    else pure text
  Updating.render $ do
    r <- view #renderer
    SDL.rendererDrawColor r $= shade2
    SDL.fillRect r (Just $ Rect.toSdl bounds)
    Rendering.text (bounds ^. #xy) text'
    SDL.rendererDrawColor r $= if focused then highlight else shade1
    SDL.drawRect r (Just $ Rect.toSdl bounds)
  pure text'

shade0, shade1, shade2, shade3, highlight :: Num a => V4 a
shade0 = V4 23 23 23 255
shade1 = V4 31 31 31 255
shade2 = V4 63 63 63 255
shade3 = V4 91 91 91 255
highlight = V4 31 171 171 255
