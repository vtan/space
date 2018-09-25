module App.Update.Updating where

import App.Prelude

import qualified App.Update.UILayout as UILayout
import qualified App.Update.UIState as UIState
import qualified SDL
import qualified SDL.Internal.Numbered

import App.Rect (Rect)
import App.Render.Rendering (Rendering)
import App.Update.Events
import App.Update.UILayout (UILayout)
import App.Update.UIState (UIState)
import App.Update.SlotId (SlotId)
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.State.Strict (runStateT)

type Updating a = ReaderT Context (StateT State Identity) a

data Context = Context
  { uiLayout :: UILayout }
  deriving (Show, Generic)

data State = State
  { events :: [SDL.Event]
  , keyModifier :: SDL.KeyModifier
  , mousePosition :: V2 Int
  , totalRealTime :: Double
  , quit :: Bool
  , reloadResources :: Bool
  , focusedWidget :: Maybe SlotId
  , activeDropdown :: Maybe (Updating ())
  , ui :: UIState
  , deferredRendering :: [Rendering ()]
  }
  deriving (Generic)

initialState :: State
initialState = State
  { events = []
  , keyModifier = SDL.Internal.Numbered.fromNumber 0
  , mousePosition = 0
  , totalRealTime = 0
  , quit = False
  , reloadResources = False
  , focusedWidget = Nothing
  , activeDropdown = Nothing
  , ui = UIState.initial
  , deferredRendering = [pure ()]
  }

runFrame :: Double -> V2 Int -> [SDL.Event] -> SDL.KeyModifier -> Context -> State -> Updating a -> (a, State)
runFrame dtime mousePos events keyMod ctx st u =
  let st' = st
        & #totalRealTime +~ dtime
        & #events .~ []
        & #keyModifier .~ keyMod
        & #mousePosition .~ mousePos
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

childLayout :: Text -> Updating a -> Updating a
childLayout childName =
  local (#uiLayout %~ UILayout.child childName)

childBounds :: Text -> (Rect Int -> Updating a) -> Updating a
childBounds childName f = do
  child <- view (#uiLayout . to (UILayout.child childName))
  f (child ^. #bounds)

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
