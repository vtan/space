module Game.WindowOverlay
  ( windowOverlay )
where

import App.Prelude

import qualified App.Logic.TimeStep as TimeStepLogic
import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widgets as Widgets
import qualified Game.UIState as UIState

import App.Common.Display (display)
import App.Common.Rect (Rect(..))
import App.Dimension.Time (Time)
import App.Model.GameState (GameState(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent, UIContext(..))
import Game.AppState (AppState(..))

windowOverlay :: AppState -> UIComponent AppState
windowOverlay _ =
  UI.cursorAt (Rect (V2 4 4) (V2 80 20)) $
    Layout.horizontal [
      Stretched $
        Widgets.button "Colonies" (onClick UIState.ColonyWindow)
    ]
  where
    onClick :: UIState.OpenWindow -> AppState -> AppState
    onClick clicked =
      over (#uiState . #openWindow) (toggle clicked)

    toggle :: UIState.OpenWindow -> Maybe UIState.OpenWindow -> Maybe UIState.OpenWindow
    toggle clicked current =
      if elem clicked current
      then Nothing
      else Just clicked