module Game.Colonies.ColonyWindow
  ( colonyWindow )
where

import GlobalImports

import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widget as Widget
import qualified Game.Bodies.Body as Body
import qualified Game.Bodies.OrbitTree as OrbitTree
import qualified Game.Bodies.Resource as Resource
import qualified Game.Bodies.ResourceOnBody as ResourceOnBody
import qualified Game.Colonies.Building as Building
import qualified Game.Colonies.BuildingLogic as BuildingLogic
import qualified Game.Colonies.MiningLogic as MiningLogic
import qualified Game.Common.Display as Display
import qualified Game.Dimension.Time as Time

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitTree (OrbitTree(..))
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Colonies.BuildOrder (BuildOrder(..))
import Game.Colonies.Colony (Colony(..))
import Game.Colonies.ColonyWindowState (ColonyWindowState(..))
import Game.Common.Display (display)
import Game.Dimension.Time (Time)

colonyWindow :: AppState -> UIComponent AppState
colonyWindow AppState{ gameState, uiState } =
  UI.cursorAt (Rect (V2 8 32) (V2 800 600) ) $
    Widget.window "Colonies" $
      Layout.horizontal
        [ Sized 200 bodyList
        , Stretched $ Layout.vertical
            [ Sized 100 (bodyPanel selectedBody selectedColony)
            , Stretched (colonyPanel selectedBody selectedColony colonyWindowState now)
            ]
        ]
  where
    now = view #time gameState
    colonyWindowState@ColonyWindowState{ selectedBodyId, bodyScrollOffset } = view #colonyWindow uiState
    selectedBody = selectedBodyId >>= \i -> view (#bodies . at i) gameState
    selectedColony = selectedBodyId >>= \i -> view (#colonies . at i) gameState

    bodiesWithLevel = OrbitTree.depthFirst (view #orbitTree gameState)
      & concatMap (\(level, OrbitTree{ bodyId }) ->
          maybeToList (view (#bodies . at bodyId) gameState) & map (level, )
        )
    bodyList = Widget.scrollList
      (Body.bodyId . snd)
      (\(level, Body{ name }) -> fold (replicate level "  │ ") <> display name)
      bodiesWithLevel
      selectedBodyId
      bodyScrollOffset
      (set (#uiState . #colonyWindow . #selectedBodyId) . Just)
      (set (#uiState . #colonyWindow . #bodyScrollOffset))

bodyPanel :: Maybe Body -> Maybe Colony -> UIComponent AppState
bodyPanel body colony =
  case body of
    Nothing -> UI.empty
    Just Body{ resources } ->
      let
        header = DefaultSized . Layout.horizontal $
          [ Sized 100 UI.empty ]
          ++ toList (fmap (const . Sized 100  $ Widget.label' "Stockpiled") colony)
          ++ [ Sized 100 (Widget.label' "Mineable")
          , Sized 100 (Widget.label' "Accessibility")
          ]
        resourceRows = Resource.all & map \resource ->
          let
            ResourceOnBody{ available, accessibility } = resources
              & view (at resource)
              & fromMaybe ResourceOnBody.empty
          in
            DefaultSized . Layout.horizontal $
              [ Sized 100 (Widget.label (display resource)) ]
              ++ (
                case colony of
                  Nothing -> []
                  Just Colony{ resources = stockpile } ->
                    let stockpiled = view (at resource . non 0) stockpile
                    in [ Sized 100 (Widget.label (Display.fixed 0 stockpiled <> " t"))]
              )
              ++ [ Sized 100 (Widget.label (Display.fixed 0 available <> " t"))
              , Sized 100 (Widget.label (Display.percent accessibility))
              ]
      in
        Layout.vertical
          [ DefaultSized (Widget.label' "Resources")
          , Stretched . Layout.indent 16 $ Layout.vertical (header : resourceRows)
          ]

colonyPanel :: Maybe Body -> Maybe Colony -> ColonyWindowState -> Time Int -> UIComponent AppState
colonyPanel bodyMay colonyMay windowState now =
  case (bodyMay, colonyMay) of
    (Just body, Just colony) ->
      Layout.vertical
        [ Sized 100 (miningPanel body colony)
        , Sized 100 (buildingPanel colony windowState now)
        ]
    _ -> UI.empty

miningPanel :: Body -> Colony -> UIComponent AppState
miningPanel Body{ resources } Colony{ buildings } =
  Layout.vertical
    [ DefaultSized (Widget.label' "Mining")
    , Stretched . Layout.indent 16 . Layout.vertical $
        [ DefaultSized $ Layout.horizontal
            [ Sized 100 UI.empty
            , Sized 100 (Widget.label' "Mines")
            , Sized 200 (Widget.label' "Monthly output")
            ]
        ]
        ++ (Resource.all & map \resource ->
            let
              mines = view (at (Building.Mine resource) . non 0) buildings
              resourceOnBody = fromMaybe ResourceOnBody.empty $ view (at resource) resources
              monthlyOutput = 30 * MiningLogic.dailyOutput mines resourceOnBody
            in
              DefaultSized $ Layout.horizontal
                [ Sized 100 (Widget.label (display resource))
                , Sized 100 (Widget.label (display mines))
                , Sized 100 (Widget.label (Display.fixed 0 monthlyOutput <> " t"))
                ]
          )
    ]

buildingPanel :: Colony -> ColonyWindowState -> Time Int -> UIComponent AppState
buildingPanel colony@Colony{ buildOrder, buildings } windowState now =
  Layout.vertical
    [ DefaultSized (Widget.label' "Building")
    , Stretched . Layout.indent 16 $ Layout.vertical
        [ DefaultSized . Widget.label $
            let factories = view (at Building.Factory . non 0) buildings
            in "Factories: " <> display factories
        , Stretched $
            case buildOrder of
              Just order -> buildingInProgressPanel order colony now
              Nothing -> buildingIdlePanel colony windowState now
        ]
    ]

buildingIdlePanel :: Colony -> ColonyWindowState -> Time Int -> UIComponent AppState
buildingIdlePanel
    colony@Colony{ bodyId }
    ColonyWindowState{ selectedBuilding, buildingQuantity }
    now =
  Layout.horizontal
    [ Sized 120 $
        Widget.list
          id
          display
          Building.all
          selectedBuilding
          (set (#uiState . #colonyWindow . #selectedBuilding))
    , Sized 64 $
        Widget.list
          id
          (\quantity -> "x" <> display quantity)
          [1, 10, 100, 1000, 10000]
          (Just buildingQuantity)
          (\q -> set (#uiState . #colonyWindow . #buildingQuantity) (fromMaybe 1 q))
    , Stretched $
        case selectedBuilding of
          Nothing -> Widget.label' "Select a building"
          Just building ->
            Layout.vertical
              [ DefaultSized . Widget.label $
                  let cost = fromIntegral buildingQuantity *^ BuildingLogic.resourceCostFor building
                  in "Resource cost: " <> Resource.displayCost cost
              , DefaultSized . Widget.label $
                  let
                    finishDate = fromMaybe "" do
                      daysToComplete <- BuildingLogic.ticksToBuild colony building buildingQuantity
                      pure $ display (Time.addDays daysToComplete now)
                  in "Finish date: " <> finishDate
              , DefaultSized $ Layout.horizontal
                  [ Stretched $ Widget.button
                      "Build"
                      ( over #gameState (BuildingLogic.start building buildingQuantity bodyId)
                      . set (#uiState . #colonyWindow . #selectedBuilding) Nothing
                      . set (#uiState . #colonyWindow . #buildingQuantity) 1
                      )
                  ]
              ]
    ]

buildingInProgressPanel :: BuildOrder -> Colony -> Time Int -> UIComponent AppState
buildingInProgressPanel
    order@BuildOrder{ target, quantity, lockedResources }
    colony@Colony{ bodyId }
    now =
  Layout.vertical
    [ DefaultSized (Widget.label ("Currently building: " <> display target <> " x" <> display quantity))
    , DefaultSized . Widget.label $
        let
          finishDate = fromMaybe "" do
            daysToComplete <- BuildingLogic.remainingTicksToComplete colony order
            pure $ display (Time.addDays daysToComplete now)
        in "Finish date: " <> finishDate
    , DefaultSized (Widget.label ("Resource cost: " <> Resource.displayCost lockedResources))
    , DefaultSized $ Widget.button
        "Cancel"
        (over #gameState (BuildingLogic.cancel order bodyId))
    ]
