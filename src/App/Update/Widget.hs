module App.Update.Widget where

import App.Prelude

import qualified App.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified App.Update.Updating as Updating
import qualified SDL

import App.Rect (Rect)
import App.Update.Events
import App.Update.Updating (Updating)

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