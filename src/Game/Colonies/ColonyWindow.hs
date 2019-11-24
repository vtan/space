module Game.Colonies.ColonyWindow
  ( colonyWindow )
where

import GlobalImports

import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widgets as Widgets
import qualified Game.Bodies.Body as Body
import qualified Game.Bodies.Resource as Resource
import qualified Game.Bodies.ResourceOnBody as ResourceOnBody
import qualified Game.Common.Display as Display

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent, UIContext(..))
import Game.AppState (AppState(..))
import Game.GameState (GameState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Common.Display (display)

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
  Just Body{ resources } ->
    let
      header = DefaultSized $ Layout.horizontal
        [ Sized 100 UI.empty
        , Sized 100 (Widgets.label' "Available")
        , Sized 100 (Widgets.label' "Accessibility")
        ]
      resourceRows = Resource.all & map \resource ->
        let
          ResourceOnBody{ available, accessibility } = resources
            & view (at resource)
            & fromMaybe ResourceOnBody.empty
        in
          DefaultSized $ Layout.horizontal
            [ Sized 100 (Widgets.label (display resource))
            , Sized 100 (Widgets.label (Display.fixed 0 available <> " t"))
            , Sized 100 (Widgets.label (Display.percent accessibility))
            ]
    in
      Layout.vertical
        [ DefaultSized (Widgets.label' "Resources")
        , Stretched . Layout.indent 16 $ Layout.vertical (header : resourceRows)
        ]
