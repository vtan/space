module App.Update.Updating where

import App.Prelude

import qualified App.Update.UIState as UIState
import qualified SDL

import App.Render.Rendering (Rendering)
import App.Update.Events
import App.Update.UIState (UIState)
import App.Update.SlotId (SlotId)
import Control.Monad.State.Strict (runStateT)

type Updating a = StateT State Identity a

data State = State
  { events :: [SDL.Event]
  , mousePosition :: V2 Int
  , totalRealTime :: Double
  , quit :: Bool
  , focusedWidget :: Maybe SlotId
  , ui :: UIState
  , deferredRendering :: Rendering ()
  }
  deriving (Generic)

initialState :: State
initialState = State
  { events = []
  , mousePosition = 0
  , totalRealTime = 0
  , quit = False
  , focusedWidget = Nothing
  , ui = UIState.initial
  , deferredRendering = pure ()
  }

runFrame :: Double -> V2 Int -> [SDL.Event] -> State -> Updating a -> (a, State)
runFrame dtime mousePos events st u =
  let st' = st
        & #totalRealTime +~ dtime
        & #events .~ []
        & #mousePosition .~ mousePos
        & #deferredRendering .~ pure ()
        & (\acc0 -> foldr (flip applyEvent) acc0 events)
      Identity (a, st'') = runStateT u st'
  in (a, st'')
  where
    applyEvent :: State -> SDL.Event -> State
    applyEvent st' = \case
      QuitEvent -> st' & #quit .~ True
      event -> st' & #events %~ (event :)

render :: Rendering () -> Updating ()
render r =
  #deferredRendering %= (*> r)

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
