module App.Update.Widget2 where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified SDL

import App.Common.Rect (Rect(..))
import App.Render.Rendering (Rendering)
import App.Update.Events (pattern MousePressEvent)
import Control.Lens (Lens')
import Data.Generics.Product (HasType, typed)
import Data.Monoid (Any(..))

data UIState = UIState
  { cursor :: V2 Int
  , widgetSize :: V2 Int
  , padding :: V2 Int
  , placementMode :: PlacementMode
  , events :: [SDL.Event]
  , renderStack :: NonEmpty (Rendering ())
  }
  deriving (Generic)

data PlacementMode
  = Vertical
  | Horizontal
  deriving (Show, Generic)

initialState :: UIState
initialState =
  UIState
    { cursor = 128
    , widgetSize = V2 80 16
    , padding = 4
    , placementMode = Vertical
    , events = []
    , renderStack = pure () :| []
    }

with :: (MonadState s m, HasType UIState s) => Lens' UIState a -> a -> m b -> m b
with prop value action = do
  oldValue <- use (typed @UIState . prop)
  typed @UIState . prop .= value
  result <- action
  typed @UIState . prop .= oldValue
  pure result

placeWidget :: (MonadState s m, HasType UIState s) => m a -> m a
placeWidget widget = do
  result <- widget
  typed @UIState %= \ui@UIState{ cursor, widgetSize, padding, placementMode } ->
    case placementMode of
      Horizontal ->
        ui{ cursor = cursor & _x +~ (widgetSize ^. _x) + (padding ^. _x) }
      Vertical ->
        ui{ cursor = cursor & _y +~ (widgetSize ^. _y) + (padding ^. _y) }
  pure result

consumeEvents :: (MonadState s m, HasType UIState s, Monoid a, Eq a) => (SDL.Event -> a) -> m a
consumeEvents p = do
  UIState{ events } <- use typed
  let (remainingEvents, results) =
        events
          & map (\e ->
              case p e of
                pe | pe == mempty -> Left e
                pe -> Right pe
            )
          & partitionEithers
  typed @UIState . #events .= remainingEvents
  pure (fold results)

render :: (MonadState s m, HasType UIState s) => Rendering () -> m ()
render r =
  typed @UIState . #renderStack %=
    \(rhead :| rtail) -> (rhead *> r) :| rtail

label :: (MonadState s m, HasType UIState s) => TextBuilder -> m ()
label =
  label' . LazyText.toStrict . TextBuilder.toLazyText

label' :: (MonadState s m, HasType UIState s) => Text -> m ()
label' text =
  placeWidget $ do
    UIState{ cursor, widgetSize } <- use typed
    render (Rendering.text (Rect cursor widgetSize) text)

button :: (MonadState s m, HasType UIState s) => Text -> m Bool
button text =
  placeWidget $ do
    UIState{ cursor, widgetSize } <- use typed
    let bounds = Rect cursor widgetSize
    Any clicked <- consumeEvents $ \case
      MousePressEvent SDL.ButtonLeft pos ->
        Any (Rect.contains bounds (fromIntegral <$> pos))
      _ -> mempty
    render $ do
      r <- view #renderer
      let color = if clicked then highlight else shade3
      SDL.rendererDrawColor r $= color
      SDL.fillRect r (Just $ Rect.toSdl bounds)
      Rendering.text bounds text
    pure clicked

shade0, shade1, shade2, shade3, highlight :: Num a => V4 a
shade0 = V4 23 23 23 255
shade1 = V4 31 31 31 255
shade2 = V4 63 63 63 255
shade3 = V4 91 91 91 255
highlight = V4 31 171 171 255
