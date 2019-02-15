module App.UI2.UI where

import App.Prelude

import qualified SDL

import App.Common.Rect (Rect(..))
import App.Common.Util (_nonEmptyHead)
import App.Render.Rendering (Rendering)
import Control.Lens (Lens')
import Data.Generics.Product (HasType, typed)

data UIState = UIState
  { groups :: NonEmpty UIGroup
  , focusedWidgetName :: Maybe Text
  , events :: [SDL.Event]
  , renderStack :: NonEmpty (Rendering ())
  , scaleFactor :: Int
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
    , scaleFactor = 4
    }

rootGroup :: UIGroup
rootGroup =
  UIGroup
    { nextWidget = Rect 32 (V2 25 5)
    , padding = 1
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

nextWidgetScaled :: (MonadState s m, HasType UIState s) => m (Rect Int)
nextWidgetScaled = do
  UIState{ scaleFactor, groups = UIGroup { nextWidget } :| _ } <- use typed
  pure (scaleFactor *^ nextWidget)
