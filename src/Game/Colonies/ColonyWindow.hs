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
import qualified Game.Colonies.Building as Building
import qualified Game.Colonies.MiningLogic as MiningLogic
import qualified Game.Common.Display as Display

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Colonies.Colony (Colony(..))
import Game.Common.Display (display)

colonyWindow :: AppState -> UIComponent AppState
colonyWindow AppState{ gameState, uiState } =
  UI.cursorAt (Rect (V2 8 32) (V2 800 600) ) $
    Widgets.window "Colonies" $
      Layout.horizontal
        [ Sized 200 bodyList
        , Stretched $ Layout.vertical
            [ Sized 120 (bodyPanel selectedBody selectedColony)
            , Stretched (colonyPanel selectedBody selectedColony)
            ]
        ]
  where
    selectedBodyId = view (#colonyWindow . #selectedBodyId) uiState
    selectedBody = selectedBodyId >>= \i -> view (#bodies . at i) gameState
    selectedColony = selectedBodyId >>= \i -> view (#colonies . at i) gameState

    bodyList = Widgets.list
      Body.bodyId
      Body.name
      (toList (view #bodies gameState))
      selectedBodyId
      (set (#uiState . #colonyWindow . #selectedBodyId))

bodyPanel :: Maybe Body -> Maybe Colony -> UIComponent AppState
bodyPanel body colony =
  case body of
    Nothing -> UI.empty
    Just Body{ resources } ->
      let
        header = DefaultSized . Layout.horizontal $
          [ Sized 100 UI.empty ]
          ++ toList (fmap (const . Sized 100  $ Widgets.label' "Stockpiled") colony)
          ++ [ Sized 100 (Widgets.label' "Mineable")
          , Sized 100 (Widgets.label' "Accessibility")
          ]
        resourceRows = Resource.all & map \resource ->
          let
            ResourceOnBody{ available, accessibility } = resources
              & view (at resource)
              & fromMaybe ResourceOnBody.empty
          in
            DefaultSized . Layout.horizontal $
              [ Sized 100 (Widgets.label (display resource)) ]
              ++ (
                case colony of
                  Nothing -> []
                  Just Colony{ resources = stockpile } ->
                    let stockpiled = view (at resource . non 0) stockpile
                    in [ Sized 100 (Widgets.label (Display.fixed 0 stockpiled <> " t"))]
              )
              ++ [ Sized 100 (Widgets.label (Display.fixed 0 available <> " t"))
              , Sized 100 (Widgets.label (Display.percent accessibility))
              ]
      in
        Layout.vertical
          [ DefaultSized (Widgets.label' "Resources")
          , Stretched . Layout.indent 16 $ Layout.vertical (header : resourceRows)
          ]

colonyPanel :: Maybe Body -> Maybe Colony -> UIComponent AppState
colonyPanel bodyMay colonyMay =
  case (bodyMay, colonyMay) of
    (Just body, Just colony) ->
      Layout.vertical [ Stretched (miningPanel body colony) ]
    _ -> UI.empty

miningPanel :: Body -> Colony -> UIComponent AppState
miningPanel Body{ resources } Colony{ buildings } =
  Layout.vertical
    [ DefaultSized (Widgets.label' "Mining")
    , Stretched . Layout.indent 16 . Layout.vertical $
        [ DefaultSized $ Layout.horizontal
            [ Sized 100 UI.empty
            , Sized 100 (Widgets.label' "Mines")
            , Sized 200 (Widgets.label' "Monthly output")
            ]
        ]
        ++ (Resource.all & map \resource ->
            let
              mines = view (at (Building.Mine resource) . non 0) buildings
              resourceOnBody = fromMaybe ResourceOnBody.empty $ view (at resource) resources
              monthlyOutput = 30 * MiningLogic.dailyOutput mines resourceOnBody
            in
              DefaultSized $ Layout.horizontal
                [ Sized 100 (Widgets.label (display resource))
                , Sized 100 (Widgets.label (display mines))
                , Sized 100 (Widgets.label (Display.fixed 0 monthlyOutput <> " t"))
                ]
          )
    ]
