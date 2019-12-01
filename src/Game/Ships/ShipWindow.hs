module Game.Ships.ShipWindow
  ( shipWindow )
where

import GlobalImports

import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widget as Widget
import qualified Game.Bodies.Resource as Resource

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))
import Game.Common.Display (display)
import Game.Ships.Ship (Ship(..))
import Game.Ships.ShipWindowState (ShipWindowState(..))

shipWindow :: AppState -> UIComponent AppState
shipWindow AppState{ gameState, uiState } =
  UI.cursorAt (Rect (V2 8 32) (V2 800 600) ) $
    Widget.window "Ships" $
      Layout.horizontal
        [ Sized 200 shipList
        , Stretched $
            case selectedShip of
              Just ship -> selectedShipPanel ship
              Nothing -> UI.empty
        ]
  where
    now = view #time gameState
    shipWindowState@ShipWindowState{ selectedShipId, selectedShipIdScroll } = view #shipWindow uiState
    selectedShip = selectedShipId >>= \i -> view (#ships . at i) gameState

    shipList = Widget.list
      (\Ship{ shipId } -> shipId)
      (\Ship{ name } -> display name)
      (toListOf (#ships . folded) gameState)
      selectedShipId
      selectedShipIdScroll
      (set (#uiState . #shipWindow . #selectedShipId) . Just)
      (set (#uiState . #shipWindow . #selectedShipIdScroll))

selectedShipPanel :: Ship -> UIComponent AppState
selectedShipPanel Ship{ speed, attachedToBody, cargoCapacity, loadedCargo } =
  Layout.vertical
    [ DefaultSized (Widget.label ("Speed: " <> display speed))
    , DefaultSized (Widget.label ("Cargo capacity: " <> display cargoCapacity <> " t"))
    , DefaultSized (Widget.label ("Loaded cargo: " <> Resource.displayWithQuantities loadedCargo))
    ]

