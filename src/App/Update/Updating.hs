module App.Update.Updating where

import App.Prelude

import qualified App.Update.UpdateState as UpdateState
import qualified Data.Semigroup as Semigroup
import qualified SDL

import App.Render.Rendering (Rendering)
import App.Update.UpdateState (UpdateState)
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Writer.CPS (runWriterT, tell)

type Updating a = StateT UpdateState (WriterT (Endo [Rendering ()]) IO) a

runFrame :: Double -> [SDL.Event] -> UpdateState -> Updating a -> IO (a, UpdateState, Rendering ())
runFrame dtime events st u =
  let st' = st
        & #totalRealTime +~ dtime
        & UpdateState.applyEvents events
  in do
    ((a, st''), rs) <- runWriterT (runStateT u st')
    pure $ (a, st'', sequence_ (appEndo rs []))

renderUI :: Rendering () -> Updating ()
renderUI ui = tell $ Semigroup.diff [ui]

consumeEvents :: (SDL.Event -> Maybe a) -> Updating [a]
consumeEvents p = do
  allEvents <- use #events
  let (remainingEvents, consumedEvents) =
        allEvents
          & map (\e -> p e & maybe (Left e) Right)
          & partitionEithers
  #events .= remainingEvents
  pure consumedEvents