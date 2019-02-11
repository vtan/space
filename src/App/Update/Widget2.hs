module App.Update.Widget2 where

import App.Prelude

import qualified App.Common.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified SDL

import App.Common.Rect (Rect(..))
import App.Common.Util (_nonEmptyHead)
import App.Render.Rendering (Rendering)
import App.Update.Events
import Control.Lens (Lens')
import Data.Generics.Product (HasType, typed)
import Data.Monoid (Any(..))

data UIState = UIState
  { groups :: NonEmpty UIGroup
  , focusedWidgetName :: Maybe Text
  , events :: [SDL.Event]
  , renderStack :: NonEmpty (Rendering ())
  }
  deriving (Generic)

data UIGroup = UIGroup
  { nextWidget :: Rect Int
  , padding :: V2 Int
  , placementMode :: PlacementMode
  , totalSize :: V2 Int
  }
  deriving (Generic)

data PlacementMode
  = Vertical
  | Horizontal
  deriving (Show, Generic)

initialState :: UIState
initialState =
  UIState
    { groups = rootGroup :| []
    , focusedWidgetName = Nothing
    , events = []
    , renderStack = pure () :| []
    }

rootGroup :: UIGroup
rootGroup =
  UIGroup
    { nextWidget = Rect 128 (V2 100 20)
    , padding = 4
    , placementMode = Vertical
    , totalSize = 0
    }

with :: forall m s a b. (MonadState s m, HasType UIState s) => Lens' UIGroup a -> a -> m b -> m b
with prop value action = do
  let lens :: Lens' s a
      lens = (typed @UIState . #groups . _nonEmptyHead . prop)
  oldValue <- use lens
  lens .= value
  result <- action
  lens .= oldValue
  pure result

sized :: (MonadState s m, HasType UIState s) => V2 Int -> m a -> m a
sized = with (#nextWidget . #wh)

group :: (MonadState s m, HasType UIState s) => PlacementMode -> m a -> m a
group placementMode child = do
  UIState{ groups = currentGroup :| prevGroups } <- use typed
  let newGroup = currentGroup{ placementMode, totalSize = 0 }
  typed @UIState . #groups %= (newGroup :|) . toList
  result <- child
  UIGroup{ totalSize } <- use (typed @UIState . #groups . _nonEmptyHead)
  typed @UIState . #groups .= advanceCursor totalSize currentGroup :| prevGroups
  pure result

placeWidget :: (MonadState s m, HasType UIState s) => m a -> m a
placeWidget widget = do
  result <- widget
  typed @UIState . #groups . _nonEmptyHead %=
    \grp@UIGroup{ nextWidget = Rect _ size } -> advanceCursor size grp
  pure result

advanceCursor :: V2 Int -> UIGroup -> UIGroup
advanceCursor size grp@UIGroup{ nextWidget, padding, placementMode, totalSize } =
  let
    mainAxis, otherAxis :: forall n. Lens' (V2 n) n
    mainAxis = case placementMode of
      Horizontal -> _x
      Vertical -> _y
    otherAxis = case placementMode of
      Horizontal -> _y
      Vertical -> _x
    mainAxisIncrement = (size ^. mainAxis) + (padding ^. mainAxis)
    nextWidget' = nextWidget & #xy . mainAxis +~ mainAxisIncrement
    totalSize' = totalSize
      & mainAxis +~ mainAxisIncrement
      & otherAxis %~ max (size ^. otherAxis)
  in grp{ nextWidget = nextWidget', totalSize = totalSize' }

consumeEvents :: (MonadState s m, HasType UIState s, Monoid a, AsEmpty a) => (SDL.Event -> a) -> m a
consumeEvents p = do
  UIState{ events } <- use typed
  let (remainingEvents, results) =
        events
          & map (\e ->
              case p e of
                Empty -> Left e
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
    UIState{ groups = UIGroup { nextWidget } :| _ } <- use typed
    render (Rendering.text nextWidget text)

button :: (MonadState s m, HasType UIState s) => Text -> m Bool
button text =
  placeWidget $ do
    UIState{ groups = UIGroup { nextWidget } :| _ } <- use typed
    Any clicked <- consumeEvents $ \case
      MousePressEvent SDL.ButtonLeft pos ->
        Any (Rect.contains nextWidget (fromIntegral <$> pos))
      _ -> mempty
    render $ do
      r <- view #renderer
      let color = if clicked then highlight else shade3
      SDL.rendererDrawColor r $= color
      SDL.fillRect r (Just $ Rect.toSdl nextWidget)
      Rendering.text nextWidget text
    pure clicked

textBox :: (MonadState s m, HasType UIState s) => Text -> Lens' s Text -> m Text
textBox name state =
  placeWidget $ do
    UIState{ groups = UIGroup { nextWidget } :| _, focusedWidgetName } <- use typed
    focused <- do
      Any clicked <- consumeEvents $ \case
        MousePressEvent SDL.ButtonLeft pos ->
          Any (Rect.contains nextWidget (fromIntegral <$> pos))
        _ -> mempty
      if clicked
      then True <$ (typed @UIState . #focusedWidgetName .= Just name)
      else pure (elem name focusedWidgetName)
    text <- use state
    text' <-
      if focused
      then do
        textMods <- consumeEvents $ \case
          TextInputEvent newText -> [(<> newText)]
          KeyPressEvent SDL.ScancodeBackspace -> [Text.dropEnd 1]
          e | isUnicodeKeyEvent e -> [id] -- consume key events for which we also had a text input event
          _ -> []
        let new = foldl' @[] (&) text textMods
        when (textMods & not . null) $
          state .= new
        pure new
      else pure text
    render $ do
      r <- view #renderer
      SDL.rendererDrawColor r $= shade2
      SDL.fillRect r (Just $ Rect.toSdl nextWidget)
      Rendering.text nextWidget text'
      SDL.rendererDrawColor r $= if focused then highlight else shade1
      SDL.drawRect r (Just $ Rect.toSdl nextWidget)
    pure text'


shade0, shade1, shade2, shade3, highlight :: Num a => V4 a
shade0 = V4 23 23 23 255
shade1 = V4 31 31 31 255
shade2 = V4 63 63 63 255
shade3 = V4 91 91 91 255
highlight = V4 31 171 171 255
