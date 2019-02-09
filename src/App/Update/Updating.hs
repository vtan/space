module App.Update.Updating where

import App.Prelude

import qualified App.Update.UIState as UIState
import qualified App.Update.WidgetTree as WidgetTree
import qualified SDL

import App.Common.HashedText (HashedText)
import App.Dimension.Time (Time)
import App.Render.Rendering (Rendering)
import App.Update.Events
import App.Update.UIState (UIState)
import App.Update.WidgetTree (WidgetTree)
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.State.Strict (runStateT)

type Updating a = ReaderT Context (StateT State Identity) a

data Context = Context
  { resources :: ResourceContext
  , frameContext :: FrameContext
  }
  deriving (Show, Generic)

data FrameContext = FrameContext
  { keyModifier :: SDL.KeyModifier
  , mousePosition :: V2 Int
  , screenSize :: V2 Int
  }
  deriving (Show, Generic)

data ResourceContext = ResourceContext
  { widgetTree :: WidgetTree }
  deriving (Show, Generic)

contextFrom :: ResourceContext -> FrameContext -> Context
contextFrom resources frameContext =
  Context{ resources, frameContext }

data State = State
  { events :: [SDL.Event]
  , timeStepPerFrame :: Maybe (Time Int)
  , movingViewport :: Bool
  , quit :: Bool
  , reloadResources :: Bool
  , focusedWidget :: Maybe HashedText
  , activeDropdown :: Maybe (Updating ())
  , ui :: UIState
  , deferredRendering :: [Rendering ()]
  }
  deriving (Generic)

initialState :: State
initialState = State
  { events = []
  , timeStepPerFrame = Nothing
  , movingViewport = False
  , quit = False
  , reloadResources = False
  , focusedWidget = Nothing
  , activeDropdown = Nothing
  , ui = UIState.initial
  , deferredRendering = [pure ()]
  }

runFrame :: [SDL.Event] -> Context -> State -> Updating a -> (a, State)
runFrame events ctx st u =
  let st' = st
        & #events .~ []
        & #deferredRendering .~ [pure ()]
        & (\acc0 -> foldr (flip applyEvent) acc0 events)
      Identity (a, st'') = runStateT (runReaderT u ctx) st'
  in (a, st'')
  where
    applyEvent :: State -> SDL.Event -> State
    applyEvent st' = \case
      QuitEvent -> st' & #quit .~ True
      event -> st' & #events %~ (event :)

render :: Rendering () -> Updating ()
render r =
  #deferredRendering . _head %= (*> r)

pushRendering :: Updating ()
pushRendering =
  #deferredRendering %= (pure () :)

useWidget :: HashedText -> Updating a -> Updating a
useWidget name =
  local (#resources . #widgetTree %~ WidgetTree.child name)

widget :: HashedText -> (WidgetTree -> Updating a) -> Updating a
widget name w =
  view (#resources . #widgetTree . to (WidgetTree.child name)) >>= w

thisWidget :: (WidgetTree -> Updating a) -> Updating a
thisWidget w =
  view (#resources . #widgetTree) >>= w

filterEvents :: (SDL.Event -> Maybe a) -> Updating [a]
filterEvents p =
  use #events <&> mapMaybe p

consumeEvents :: (SDL.Event -> Maybe a) -> Updating [a]
consumeEvents p = do
  allEvents <- use #events
  let (remainingEvents, consumedEvents) =
        allEvents
          & map (\e -> p e & maybe (Left e) Right)
          & partitionEithers
  #events .= remainingEvents
  pure consumedEvents
