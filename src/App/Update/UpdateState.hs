module App.Update.UpdateState where

import App.Prelude

import qualified SDL

import App.Update.Events

data UpdateState = UpdateState
  { events :: [SDL.Event]
  , totalRealTime :: Double
  , quit :: Bool
  }
  deriving (Show, Generic)

initial :: UpdateState
initial = UpdateState
  { events = []
  , totalRealTime = 0
  , quit = False
  }

applyEvents :: [SDL.Event] -> UpdateState -> UpdateState
applyEvents events st =
  let st0 = st { events = [] }
  in foldr (flip applyEvent) st0 events

applyEvent :: UpdateState -> SDL.Event -> UpdateState
applyEvent st = \case
  QuitEvent -> st & #quit .~ True
  e@MousePressEvent{} -> st & #events %~ (e :)
  e@MouseReleaseEvent{} -> st & #events %~ (e :)
  e@MouseMotionEvent{} -> st & #events %~ (e :)
  e@MouseWheelEvent{} -> st & #events %~ (e :)
  e@KeyPressEvent{} -> st & #events %~ (e :)
  _ -> st