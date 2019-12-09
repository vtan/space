module Game.Ships.ShipWindow
  ( shipWindow )
where

import GlobalImports

import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widget as Widget
import qualified Game.GameState as GameState
import qualified Game.Bodies.BodyList as BodyList
import qualified Game.Bodies.Resource as Resource
import qualified Game.Ships.ShipLogic as ShipLogic

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))
import Game.Bodies.Body (Body(..))
import Game.Common.Display (display)
import Game.Ships.PlottedPath (PlottedPath(..))
import Game.Ships.Ship (Ship(..), ShipMovement(..))
import Game.Ships.ShipWindowState (ShipWindowState(..))

shipWindow :: AppState -> UIComponent AppState
shipWindow appState@AppState{ gameState, uiState } =
  UI.cursorAt (Rect (V2 8 32) (V2 800 600) ) $
    Widget.window "Ships" $
      Layout.horizontal
        [ Sized 200 shipList
        , Stretched $
            case selectedShip of
              Just ship -> selectedShipPanel appState ship
              Nothing -> UI.empty
        ]
  where
    ShipWindowState{ selectedShipId, selectedShipIdScroll } = view #shipWindow uiState
    selectedShip = selectedShipId >>= \i -> view (#ships . at i) gameState

    shipList = Widget.list
      (\Ship{ shipId } -> shipId)
      (\Ship{ name } -> display name)
      (toListOf (#ships . folded) gameState)
      selectedShipId
      selectedShipIdScroll
      (set (#uiState . #shipWindow . #selectedShipId) . Just)
      (set (#uiState . #shipWindow . #selectedShipIdScroll))

selectedShipPanel :: AppState -> Ship -> UIComponent AppState
selectedShipPanel appState ship@Ship{ speed, cargoCapacity, loadedCargo, movement } =
  Layout.vertical
    [ DefaultSized (Widget.label ("Speed: " <> display speed))
    , DefaultSized (Widget.label ("Cargo capacity: " <> display cargoCapacity <> " t"))
    , DefaultSized (Widget.label ("Loaded cargo: " <> Resource.displayWithQuantities loadedCargo))
    , DefaultSized (Widget.label' "Current order")
    , Stretched . Layout.indent 16 $
        case movement of
          Just _ -> currentOrderPanel appState ship
          Nothing -> newOrderPanel appState ship
    ]

currentOrderPanel :: AppState -> Ship -> UIComponent AppState
currentOrderPanel AppState{ gameState } Ship{ shipId, movement } =
  Layout.vertical $
    case movement of
      Just ShipMovement{ bodyId, path = PlottedPath{ endTime }} ->
        [ DefaultSized . Widget.label $
            let Body{ name } = GameState.expectBody bodyId gameState
            in "Flying to " <> display name <> ", arriving at " <> display endTime
        , DefaultSized $ Widget.button
            "Cancel order"
            (set (#gameState . #ships . at shipId . _Just . #movement) Nothing)
        ]
      Nothing -> []

newOrderPanel :: AppState -> Ship -> UIComponent AppState
newOrderPanel appState ship =
  Layout.horizontal
    [ Sized 100 $ BodyList.bodyList
        (#uiState . #shipWindow . #selectedBodyId)
        (#uiState . #shipWindow . #selectedBodyIdScroll)
        appState
    , Sized 100 $ Layout.vertical
        [ case selectedBodyId of
            Just bodyId ->
              DefaultSized $ Widget.button "Fly" (over #gameState (ShipLogic.setMovement ship bodyId))
            Nothing ->
              DefaultSized UI.empty
        ]
    ]
  where
    ShipWindowState{ selectedBodyId } = view (#uiState . #shipWindow) appState
