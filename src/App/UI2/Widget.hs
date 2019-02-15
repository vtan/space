module App.UI2.Widget where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified App.UI2.UI as UI
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified SDL

import App.UI2.UI (UIState(..))
import App.Update.Events
import Control.Lens (Lens')
import Data.Generics.Product (HasType, typed)
import Data.Monoid (Any(..))

label :: (MonadState s m, HasType UIState s) => TextBuilder -> m ()
label =
  label' . LazyText.toStrict . TextBuilder.toLazyText

label' :: (MonadState s m, HasType UIState s) => Text -> m ()
label' text =
  UI.placeWidget $ do
    bounds <- UI.nextWidgetScaled
    UI.render (Rendering.text bounds text)

button :: (MonadState s m, HasType UIState s) => Text -> m Bool
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
      Rendering.text bounds text
    pure clicked

textBox :: (MonadState s m, HasType UIState s) => Text -> Lens' s Text -> m Text
textBox name state =
  UI.placeWidget $ do
    UIState{ focusedWidgetName } <- use typed
    bounds <- UI.nextWidgetScaled
    focused <- do
      Any clicked <- UI.consumeEvents $ \case
        MousePressEvent SDL.ButtonLeft pos ->
          Any (Rect.contains bounds (fromIntegral <$> pos))
        _ -> mempty
      if clicked
      then True <$ (typed @UIState . #focusedWidgetName .= Just name)
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
      Rendering.text bounds text'
      SDL.rendererDrawColor r $= if focused then highlight else shade1
      SDL.drawRect r (Just $ Rect.toSdl bounds)
    pure text'

shade0, shade1, shade2, shade3, highlight :: Num a => V4 a
shade0 = V4 23 23 23 255
shade1 = V4 31 31 31 255
shade2 = V4 63 63 63 255
shade3 = V4 91 91 91 255
highlight = V4 31 171 171 255
