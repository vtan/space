module Game.Controller
  ( update )
where

import App.Prelude

import qualified App.Logic.TimeStep as TimeStepLogic
import qualified Core.UI.UI as UI
import qualified Game.SystemMap.Component as SystemMap
import qualified Game.TimeOverlay as TimeOverlay

import App.Common.Rect (Rect(..))
import Core.CoreContext (CoreContext(..))
import Core.UI.Theme (Theme(..))
import Core.UI.UI (UIComponent, UIContext(..), UIState(..))
import Game.AppState (AppState(..))

import qualified SDL

update :: [SDL.Event] -> AppState -> (ReaderT CoreContext IO (), AppState)
update events appState@AppState{ timeStep } =
  let
    uiContext = UIContext
      { cursor = Rect 0 0
      , defaultSize = V2 80 20
      , layoutGap = 4
      , scaleFactor = 1
      , theme = Theme
        { borderColor = V4 191 191 191 255
        , highlightColor = V4 31 171 171 255
        }
      }
    (stateChangeFromUi, UIState{ renderStack }) = UI.run uiContext events (ui appState)
    appState' = appState
      & stateChangeFromUi
      & ( case timeStep of
            Just time -> over #gameState (TimeStepLogic.stepTime time)
            Nothing -> id
        )
  in
    ( sequence_ (toList renderStack)
    , appState'
    )

ui :: AppState -> UIComponent AppState
ui appState =
  UI.concat
    [ TimeOverlay.timeOverlay appState
    , UI.pushRenderStack *> SystemMap.systemMap appState
    ]
