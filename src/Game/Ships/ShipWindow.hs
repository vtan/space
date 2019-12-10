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
import Game.Ships.Ship (Ship(..), ShipMovement(..), ShipOrder(..))
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
selectedShipPanel appState ship@Ship{ speed, cargoCapacity, loadedCargo, movement, order } =
  Layout.vertical
    [ DefaultSized (Widget.label ("Speed: " <> display speed))
    , DefaultSized (Widget.label ("Cargo capacity: " <> display cargoCapacity <> " t"))
    , DefaultSized (Widget.label ("Loaded cargo: " <> Resource.displayWithQuantities loadedCargo))
    , DefaultSized (Widget.label' "Current order")
    , Stretched . Layout.indent 16 $
        case (movement, order) of
          (Nothing, Nothing) -> newOrderPanel appState ship
          _ -> currentOrderPanel appState ship
    ]

currentOrderPanel :: AppState -> Ship -> UIComponent AppState
currentOrderPanel AppState{ gameState } ship@Ship{ movement, order } =
  Layout.vertical (movementLines ++ orderLines ++ cancelLine)
  where
    movementLines = case movement of
      Nothing -> []
      Just ShipMovement{ destinationBodyId, path = PlottedPath{ endTime }} ->
        [ DefaultSized . Widget.label $
            let Body{ name } = GameState.expectBody destinationBodyId gameState
            in "Flying to " <> display name <> ", arriving at " <> display endTime
        ]

    orderLines = case order of
      Nothing -> []
      Just Colonize{ endTime } ->
        [ DefaultSized . Widget.label $
            "Colonizing, finishing at " <> display endTime
        ]

    cancelLine =
      [ DefaultSized $ Widget.button
          "Cancel order"
          (over #gameState (ShipLogic.cancelOrder ship))
      ]

newOrderPanel :: AppState -> Ship -> UIComponent AppState
newOrderPanel appState ship =
  Layout.horizontal
    [ Sized 120 $ BodyList.bodyList
        (#uiState . #shipWindow . #selectedBodyId)
        (#uiState . #shipWindow . #selectedBodyIdScroll)
        appState

    , Sized 120 $ Widget.list
        id
        display
        [minBound .. maxBound]
        selectedOrderType
        selectedOrderTypeScroll
        (set (#uiState . #shipWindow . #selectedOrderType) . Just)
        (set (#uiState . #shipWindow . #selectedOrderTypeScroll))

    , Sized 180 $ Layout.vertical
        [ DefaultSized $ case (selectedBodyId, selectedOrderType) of
            (Just bodyId, Just orderType) ->
              Widget.button
                "Make order"
                (over #gameState (ShipLogic.makeOrder ship bodyId orderType))
            (Just _, Nothing) ->
              Widget.label' "Select an order"
            _ ->
              Widget.label' "Select a destination"
        ]
    ]
  where
    ShipWindowState{ selectedBodyId, selectedOrderType, selectedOrderTypeScroll } =
      view (#uiState . #shipWindow) appState
