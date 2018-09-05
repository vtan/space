module App.Update.Widget where

import App.Prelude

import qualified App.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified App.Update.Updating as Updating
import qualified Data.Text as Text
import qualified SDL

import App.Rect (Rect)
import App.Update.Events
import App.Update.Updating (Updating)
import App.Update.SlotId (SlotId)

label :: V2 Int -> Text -> Updating ()
label pos text = Updating.renderUI $ Rendering.text pos text

labels :: V2 Int -> Int -> [Text] -> Updating ()
labels firstPos verticalSpacing texts =
  Updating.renderUI $
    ifor_ texts $ \i text ->
      let pos = firstPos & _y +~ i * verticalSpacing
      in Rendering.text pos text

button :: Rect Int -> Text -> Updating Bool
button bounds text = do
  clicked <- Updating.consumeEvents (\case
      MousePressEvent SDL.ButtonLeft pos | Rect.contains bounds (fromIntegral <$> pos) -> Just ()
      _ -> Nothing
    ) <&> (not . null)
  Updating.renderUI $ do
    r <- view #renderer
    let color = if clicked then V4 31 171 171 255 else V4 71 71 71 255
    SDL.rendererDrawColor r $= color
    SDL.fillRect r (Just $ Rect.toSdl bounds)
    Rendering.text (bounds ^. #xy) text
  pure clicked

listBox :: Eq i => Rect Int -> Int -> (a -> i) -> (a -> Text) -> [a] -> Maybe i -> Updating (Maybe a, Maybe a)
listBox bounds verticalSpacing toIx toText items selectedIx = do
  clickedPos <- Updating.consumeEvents (\case
      MousePressEvent SDL.ButtonLeft pos | Rect.contains bounds (fromIntegral <$> pos) ->
        Just (fromIntegral <$> pos)
      _ -> Nothing
    ) <&> listToMaybe
  let clickedRow = clickedPos <&> \pos ->
        quot ((pos - (bounds ^. #xy)) ^. _y) verticalSpacing
      clickedItem = clickedRow >>= \i -> items ^? ix i
      selectedIx' = fmap toIx clickedItem <|> selectedIx
      (selectedItem, selectedRow, texts) = (\b as f -> ifoldr f b as) (Nothing, Nothing, []) items $ \row item (accItem, accRow, accTexts) ->
        -- TODO ^ ugly
        if elem (toIx item) selectedIx'
        then (Just item, Just row, toText item : accTexts)
        else (accItem, accRow, toText item : accTexts)
  Updating.renderUI $ do
    r <- view #renderer
    SDL.rendererDrawColor r $= V4 31 31 31 255
    SDL.fillRect r (Just $ Rect.toSdl bounds)
    for_ selectedRow $ \row -> do
      let rect = bounds
            & #xy . _y +~ row * verticalSpacing
            & #wh . _y .~ verticalSpacing
      SDL.rendererDrawColor r $= V4 31 171 171 255
      SDL.fillRect r (Just $ Rect.toSdl rect)
    ifor_ texts $ \row text ->
      let pos = (bounds ^. #xy) & _y +~ row * verticalSpacing
      in Rendering.text pos text
  pure (selectedItem, clickedItem)

window :: Rect Int -> Int -> Text -> Updating ()
window bounds titleHeight title =
  Updating.renderUI $ do
    let titleBounds = bounds & #wh . _y .~ titleHeight
        restBounds = bounds 
          & #xy . _y +~ titleHeight
          & #wh . _y -~ titleHeight
    r <- view #renderer
    SDL.rendererDrawColor r $= V4 63 63 63 255
    SDL.fillRect r (Just $ Rect.toSdl titleBounds)
    SDL.rendererDrawColor r $= V4 23 23 23 255
    SDL.fillRect r (Just $ Rect.toSdl restBounds)
    Rendering.text (bounds ^. #xy) title

textBox :: SlotId -> Rect Int -> Text -> Updating Text
textBox slotId bounds text = do
  focused <- do
    clicked <- Updating.consumeEvents (\case
        MousePressEvent SDL.ButtonLeft pos | Rect.contains bounds (fromIntegral <$> pos) -> Just ()
        _ -> Nothing
      ) <&> (not . null)
    if clicked
    then True <$ (#focusedWidget .= Just slotId)
    else elem slotId <$> use #focusedWidget
  text' <- if focused
    then do
      textMods <- Updating.consumeEvents $ \case
        TextInputEvent newText -> Just (<> newText)
        KeyPressEvent SDL.ScancodeBackspace -> Just (Text.dropEnd 1)
        e | isUnicodeKeyEvent e -> Just id -- consume key events for which we also had a text input event
        _ -> Nothing
      pure $ foldl' (&) text textMods
    else pure text
  Updating.renderUI $ do
    r <- view #renderer
    SDL.rendererDrawColor r $= V4 63 63 63 255
    SDL.fillRect r (Just $ Rect.toSdl bounds)
    Rendering.text (bounds ^. #xy) text'
    SDL.rendererDrawColor r $= if focused then V4 31 171 171 255 else V4 31 31 31 255
    SDL.drawRect r (Just $ Rect.toSdl bounds)
  pure text'