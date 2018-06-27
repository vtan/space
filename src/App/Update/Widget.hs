module App.Update.Widget where

import App.Prelude

import qualified App.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified App.Update.Updating as Updating
import qualified SDL

import App.Rect (Rect)
import App.Update.Events
import App.Update.Updating (Updating)

labels :: V2 Int -> Int -> [Text] -> Updating ()
labels firstPos verticalSpacing texts =
  Updating.renderUI $ do
    ifor_ texts $ \i text ->
      let pos = firstPos & _y +~ i * verticalSpacing
      in Rendering.text pos text

listBox :: Eq i => Rect Int -> Int -> (a -> i) -> (a -> Text) -> Maybe i -> [a] -> Updating (Maybe a)
listBox bounds verticalSpacing toIx toText selectedIx items = do
  clickedPos <- Updating.consumeEvents (\case
      MousePressEvent SDL.ButtonLeft pos -> Rect.contains bounds (fromIntegral <$> pos)
      _ -> False
    ) <&> \case -- TODO ugly
      MousePressEvent SDL.ButtonLeft pos : _ -> Just (fromIntegral <$> pos)
      _ -> Nothing
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
  pure selectedItem

selectable :: Bool -> Rect Int -> Text -> Updating Bool
selectable selected rect text = do
  clicked <- Updating.consumeEvents (\case
      MousePressEvent SDL.ButtonLeft pos -> Rect.contains rect (fromIntegral <$> pos)
      _ -> False
    ) <&> (not . null)
  let selected' = selected || clicked
  Updating.renderUI $ do
    let bg = if selected' then V4 31 171 171 255 else V4 31 31 31 255
    r <- view #renderer
    SDL.rendererDrawColor r $= bg
    SDL.fillRect r (Just $ Rect.toSdl rect)
    Rendering.text (rect ^. #xy) text
  pure clicked