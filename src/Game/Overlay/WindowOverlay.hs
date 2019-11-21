module Game.Overlay.WindowOverlay
  ( windowOverlay )
where

import GlobalImports

import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widgets as Widgets
import qualified Game.UIState as UIState

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent)
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
