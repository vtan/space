module App.Update.Updating where

import App.Prelude

import qualified App.Update.UIState as UIState
import qualified Data.Semigroup as Semigroup
import qualified SDL

import App.Render.Rendering (Rendering)
import App.Update.Events
import App.Update.UIState (UIState)
import App.Update.SlotId (SlotId)
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.CPS (runWriterT)

type Updating a = StateT State (WriterT (Endo [Rendering ()]) Identity) a

data State = State
  { events :: [SDL.Event]
  , totalRealTime :: Double
  , quit :: Bool
  , focusedWidget :: Maybe SlotId
  , ui :: UIState
  }
  deriving (Generic)

initialState :: State
initialState = State
  { events = []
  , totalRealTime = 0
  , quit = False
  , focusedWidget = Nothing
  , ui = UIState.initial
  }

runFrame :: Double -> [SDL.Event] -> State -> Updating a -> (a, State, Rendering ())
runFrame dtime events st u =
  let st' = st
        & #totalRealTime +~ dtime
        & #events .~ []
        & (\acc0 -> foldr (flip applyEvent) acc0 events)
      Identity ((a, st''), rs) = runWriterT (runStateT u st')
  in (a, st'', sequence_ (appEndo rs []))
  where
    applyEvent :: State -> SDL.Event -> State
    applyEvent st' = \case
      QuitEvent -> st' & #quit .~ True
      event -> st' & #events %~ (event :)

renderUI :: Rendering () -> Updating ()
renderUI ui = tell $ Semigroup.diff [ui]

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