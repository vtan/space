module Game.ColonyWindow
  ( colonyWindow )
where

import App.Prelude

import qualified App.Logic.TimeStep as TimeStepLogic
import qualified App.Model.Body as Body
import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widgets as Widgets

import App.Common.Display (display)
import App.Common.Rect (Rect(..))
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.GameState (GameState(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent, UIContext(..))
import Game.AppState (AppState(..))

colonyWindow :: AppState -> UIComponent AppState
colonyWindow AppState{ gameState, uiState } =
  UI.cursorAt (Rect (V2 8 32) (V2 800 600) ) $
    Widgets.window "Colonies" $
      Layout.horizontal
        [ Sized 200 bodyList
        , Stretched $ Layout.vertical
            [ Sized 200 (bodyPanel selectedBody) ]
        ]
  where
    selectedBodyId = view (#colonyWindow . #selectedBodyId) uiState
    selectedBody = selectedBodyId >>= \i -> view (#bodies . at i) gameState

    bodyList = Widgets.list
      Body.bodyId
      Body.name
      (toList (view #bodies gameState))
      selectedBodyId
      (set (#uiState . #colonyWindow . #selectedBodyId))

bodyPanel :: Maybe Body -> UIComponent AppState
bodyPanel = \case
  Nothing -> UI.empty
  Just Body{ name } -> Widgets.label' name