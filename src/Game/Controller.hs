module Game.Controller
  ( update )
where

import GlobalImports

import qualified Core.UI.Theme as Theme
import qualified Core.UI.UI as UI
import qualified Game.UIState as UIState
import qualified Game.Colonies.ColonyWindow as ColonyWindow
import qualified Game.Overlay.TimeOverlay as TimeOverlay
import qualified Game.Overlay.WindowOverlay as WindowOverlay
import qualified Game.Ships.ShipWindow as ShipWindow
import qualified Game.SystemMap.Component as SystemMap
import qualified Game.TimeLogic as TimeLogic

import Core.CoreContext (CoreContext(..))
import Core.Common.Rect (Rect(..))
import Core.UI.UI (UIComponent, UIContext(..), UIState(..))
import Game.AppState (AppState(..))

import qualified SDL

update :: V2 Int -> V2 CInt  -> [SDL.Event] -> AppState -> (ReaderT CoreContext IO (), AppState)
update screenSize mousePosition events appState@AppState{ timeStep } =
  let
    scaleFactor = 1
    uiContext = UIContext
      { cursor = Rect 0 0
      , defaultSize = V2 80 20
      , layoutGap = 4
      , scaleFactor = scaleFactor
      , scaledScreenSize = fmap fromIntegral screenSize ^/ scaleFactor
      , scaledMousePosition = fmap fromIntegral mousePosition ^/ scaleFactor
      , theme = Theme.theme
      }
    (stateChangeFromUi, UIState{ renderStack }) = UI.run uiContext events (ui appState)
    appState' = appState
      & stateChangeFromUi
      & ( case timeStep of
            Just time -> over #gameState (TimeLogic.stepTime time)
            Nothing -> id
        )
  in
    ( sequence_ (toList renderStack)
    , appState'
    )

ui :: AppState -> UIComponent AppState
ui appState =
  UI.concat
    [ WindowOverlay.windowOverlay appState
    , TimeOverlay.timeOverlay appState
    , openWindow appState
    , UI.pushRenderStack *> SystemMap.systemMap appState
    ]
  where
    openWindow = case view (#uiState . #openWindow) appState of
      Just UIState.ColonyWindow -> ColonyWindow.colonyWindow
      Just UIState.ShipWindow -> ShipWindow.shipWindow
      Nothing -> const UI.empty

