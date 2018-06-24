module App.Update.Updating where

import App.Prelude

import qualified App.Update.UpdateState as UpdateState
import qualified Data.List as List
import qualified SDL

import App.Render.Rendering (Rendering)
import App.Update.UpdateState (UpdateState)
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Writer.CPS (runWriterT)

type Updating a = StateT UpdateState (WriterT [Rendering ()] Identity) a

runFrame :: Double -> [SDL.Event] -> UpdateState -> Updating a -> (a, UpdateState, Rendering ())
runFrame dtime events st u =
  let st' = st
        & #totalRealTime +~ dtime
        & UpdateState.applyEvents events
      Identity ((a, st''), rs) = runWriterT (runStateT u st')
  in (a, st'', sequence_ rs)

consumeEvents :: (SDL.Event -> Bool) -> Updating [SDL.Event]
consumeEvents p = do
  allEvents <- use #events
  let (consumedEvents, remainingEvents) = List.partition p allEvents
  #events .= remainingEvents
  pure consumedEvents