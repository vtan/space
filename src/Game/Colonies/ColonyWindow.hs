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
import qualified Game.Colonies.BuildingLogic as BuildingLogic
import qualified Game.Colonies.MiningLogic as MiningLogic
import qualified Game.Common.Display as Display
import qualified Game.Dimension.Time as Time

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Colonies.BuildOrder (BuildOrder(..))
import Game.Colonies.Colony (Colony(..))
import Game.Colonies.ColonyWindowState (ColonyWindowState(..))
import Game.Common.Display (display)
import Game.Dimension.Time (Time)

colonyWindow :: AppState -> UIComponent AppState
colonyWindow AppState{ gameState, uiState } =
  UI.cursorAt (Rect (V2 8 32) (V2 800 600) ) $
    Widgets.window "Colonies" $
      Layout.horizontal
        [ Sized 200 bodyList
        , Stretched $ Layout.vertical
            [ Sized 120 (bodyPanel selectedBody selectedColony)
            , Stretched (colonyPanel selectedBody selectedColony colonyWindowState now)
            ]
        ]
  where
    now = view #time gameState
    colonyWindowState@ColonyWindowState{ selectedBodyId } = view #colonyWindow uiState
    selectedBody = selectedBodyId >>= \i -> view (#bodies . at i) gameState
    selectedColony = selectedBodyId >>= \i -> view (#colonies . at i) gameState

    bodyList = Widgets.list
      Body.bodyId
      (display . Body.name)
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

colonyPanel :: Maybe Body -> Maybe Colony -> ColonyWindowState -> Time Int -> UIComponent AppState
colonyPanel bodyMay colonyMay windowState now =
  case (bodyMay, colonyMay) of
    (Just body, Just colony) ->
      Layout.vertical
        [ Sized 200 (miningPanel body colony)
        , Sized 100 (buildingPanel colony windowState now)
        ]
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

buildingPanel :: Colony -> ColonyWindowState -> Time Int -> UIComponent AppState
buildingPanel colony@Colony{ buildOrder, buildings } windowState now =
  Layout.vertical
    [ DefaultSized (Widgets.label' "Building")
    , Stretched . Layout.indent 16 $ Layout.vertical
        [ DefaultSized . Widgets.label $
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
        Widgets.list
          id
          display
          Building.all
          selectedBuilding
          (set (#uiState . #colonyWindow . #selectedBuilding))
    , Sized 64 $
        Widgets.list
          id
          (\quantity -> "x" <> display quantity)
          [1, 10, 100, 1000, 10000]
          (Just buildingQuantity)
          (\q -> set (#uiState . #colonyWindow . #buildingQuantity) (fromMaybe 1 q))
    , Stretched $
        case selectedBuilding of
          Nothing -> Widgets.label' "Select a building"
          Just building ->
            Layout.vertical
              [ DefaultSized . Widgets.label $
                  let cost = fromIntegral buildingQuantity *^ BuildingLogic.resourceCostFor building
                  in "Resource cost: " <> Resource.displayCost cost
              , DefaultSized . Widgets.label $
                  let
                    finishDate = fromMaybe "" do
                      daysToComplete <- BuildingLogic.ticksToBuild colony building buildingQuantity
                      pure $ display (Time.addDays daysToComplete now)
                  in "Finish date: " <> finishDate
              , DefaultSized $ Layout.horizontal
                  [ Stretched $ Widgets.button
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
    [ DefaultSized (Widgets.label ("Currently building: " <> display target <> " x" <> display quantity))
    , DefaultSized . Widgets.label $
        let
          finishDate = fromMaybe "" do
            daysToComplete <- BuildingLogic.remainingTicksToComplete colony order
            pure $ display (Time.addDays daysToComplete now)
        in "Finish date: " <> finishDate
    , DefaultSized (Widgets.label ("Resource cost: " <> Resource.displayCost lockedResources))
    , DefaultSized $ Widgets.button
        "Cancel"
        (over #gameState (BuildingLogic.cancel order bodyId))
    ]
