module App.Update.Updating where

import App.Prelude

import qualified App.Update.UpdateState as UpdateState
import qualified SDL

import App.Update.UpdateState (UpdateState)
import Control.Monad.State.Strict (runStateT)

type Updating a = StateT UpdateState Identity a

runFrame :: Double -> [SDL.Event] -> UpdateState -> Updating a -> (a, UpdateState)
runFrame dtime events st u =
  let st' = st
        & #totalRealTime +~ dtime
        & UpdateState.applyEvents events
  in runIdentity $ runStateT u st'