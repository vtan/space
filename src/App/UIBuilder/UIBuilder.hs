module App.UIBuilder.UIBuilder where

import App.Prelude

import qualified SDL

import App.Common.Rect (Rect(..))
import App.Common.Util (_nonEmptyHead)
import App.Render.Rendering (Rendering)
import App.UIBuilder.Unscaled
import Control.Lens (Lens')
import Data.Generics.Product (HasType, typed)

type MonadUI r s m =
  (MonadReader r m, MonadState s m, HasType UIBuilderContext r, HasType UIBuilderState s)

data UIBuilderState = UIBuilderState
  { groups :: NonEmpty UIGroup
  , focusedWidgetName :: Maybe Text
  , events :: [SDL.Event]
  , renderStack :: NonEmpty (Rendering ())
  }
  deriving (Generic)

data UIBuilderContext = UIBuilderContext
  { keyModifier :: SDL.KeyModifier
  , mousePosition :: V2 Int
  , screenSize :: V2 Int
  , scaleFactor :: Int
  }
  deriving (Show, Generic)

data UIGroup = UIGroup
  { nextWidget :: Rect (Unscaled Int)
  , padding :: V2 (Unscaled Int)
  , placementMode :: PlacementMode
  , totalSize :: V2 (Unscaled Int)
  }
  deriving (Generic)

data PlacementMode
  = Vertical
  | Horizontal
  deriving (Show, Generic)

initialState :: UIBuilderState
initialState =
  UIBuilderState
    { groups = rootGroup :| []
    , focusedWidgetName = Nothing
    , events = []
    , renderStack = pure () :| []
    }

rootGroup :: UIGroup
rootGroup =
  UIGroup
    { nextWidget = Rect 0 (V2 25 5)
    , padding = 1
    , placementMode = Vertical
    , totalSize = 0
    }

with :: forall m r s a b. MonadUI r s m => Lens' UIGroup a -> a -> m b -> m b
with prop value action = do
  let lens :: Lens' s a
      lens = (typed @UIBuilderState . #groups . _nonEmptyHead . prop)
  oldValue <- use lens
  lens .= value
  result <- action
  lens .= oldValue
  pure result

positioned :: MonadUI r s m => V2 (Unscaled Int) -> m a -> m a
positioned = with (#nextWidget . #xy)

sized :: MonadUI r s m => V2 (Unscaled Int) -> m a -> m a
sized = with (#nextWidget . #wh)

width :: MonadUI r s m => Unscaled Int -> m a -> m a
width = with (#nextWidget . #wh . _x)

height :: MonadUI r s m => Unscaled Int -> m a -> m a
height = with (#nextWidget . #wh . _y)

padded :: MonadUI r s m => V2 (Unscaled Int) -> m a -> m a
padded = with #padding

group :: MonadUI r s m => PlacementMode -> m a -> m a
group placementMode child = do
  UIBuilderState{ groups = currentGroup :| prevGroups } <- use typed
  let newGroup = currentGroup{ placementMode, totalSize = 0 }
  typed @UIBuilderState . #groups %= (newGroup :|) . toList
  result <- child
  UIGroup{ totalSize } <- use (typed @UIBuilderState . #groups . _nonEmptyHead)
  typed @UIBuilderState . #groups .= advanceCursor totalSize currentGroup :| prevGroups
  pure result

group' :: MonadUI r s m => m a -> m a
group' child = do
  UIBuilderState{ groups = UIGroup{ placementMode } :| _ } <- use typed
  group placementMode child

placeWidget :: MonadUI r s m => m a -> m a
placeWidget widget = do
  result <- widget
  typed @UIBuilderState . #groups . _nonEmptyHead %=
    \grp@UIGroup{ nextWidget = Rect _ size } -> advanceCursor size grp
  pure result

advanceCursor :: V2 (Unscaled Int) -> UIGroup -> UIGroup
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

consumeEvents :: (MonadUI r s m, Monoid a, AsEmpty a) => (SDL.Event -> a) -> m a
consumeEvents p = do
  UIBuilderState{ events } <- use typed
  let (remainingEvents, results) =
        events
          & map (\e ->
              case p e of
                Empty -> Left e
                pe -> Right pe
            )
          & partitionEithers
  typed @UIBuilderState . #events .= remainingEvents
  pure (fold results)

render :: MonadUI r s m => Rendering () -> m ()
render r =
  typed @UIBuilderState . #renderStack %=
    \(rhead :| rtail) -> (rhead *> r) :| rtail

nextWidgetScaled :: MonadUI r s m => m (Rect Int)
nextWidgetScaled = do
  sc <- scaler
  UIBuilderState{ groups = UIGroup { nextWidget } :| _ } <- use typed
  pure (fmap sc nextWidget)

scaler :: MonadUI r s m => m (Unscaled Int -> Int)
scaler = do
  UIBuilderContext{ scaleFactor } <- view typed
  pure $ \(Unscaled x) -> x * scaleFactor

startOfRightAligned :: MonadUI r s m => Int -> m (Unscaled Int)
startOfRightAligned groupWidth = do
  UIBuilderContext{ scaleFactor, screenSize } <- view typed
  let V2 screenWidth _ = fmap (`div` scaleFactor) screenSize
  pure (Unscaled (screenWidth - groupWidth))
