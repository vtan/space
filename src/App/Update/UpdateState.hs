module App.Update.UpdateState where

import App.Prelude

import qualified App.Update.UIState as UIState
import qualified SDL

import App.Update.Events
import App.Update.UIState (UIState)
import App.Update.SlotId (SlotId)

data UpdateState = UpdateState
  { events :: [SDL.Event]
  , totalRealTime :: Double
  , quit :: Bool
  , focusedWidget :: Maybe SlotId
  , ui :: UIState
  }
  deriving (Generic)

initial :: UpdateState
initial = UpdateState
  { events = []
  , totalRealTime = 0
  , quit = False
  , focusedWidget = Nothing
  , ui = UIState.initial
  }

applyEvents :: [SDL.Event] -> UpdateState -> UpdateState
applyEvents events st =
  let st0 = st { events = [] }
  in foldr (flip applyEvent) st0 events

applyEvent :: UpdateState -> SDL.Event -> UpdateState
applyEvent st = \case
  QuitEvent -> st & #quit .~ True
  event -> st & #events %~ (event :)