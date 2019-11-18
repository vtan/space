module Game.SystemMap.Component where

import App.Prelude

import qualified App.Common.Camera as Camera
import qualified Core.UI.UI as UI
import qualified Game.SystemMap.Rendering as SystemMap.Rendering

import App.Common.EventPatterns
import App.Common.Util (clamp)
import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))
import Game.UIState (UIState(..))

import qualified SDL

import Control.Monad.State.Class (modify')
import Data.Monoid (Endo(..))
import Numeric.Extras (cbrt)

systemMap :: AppState -> UIComponent AppState
systemMap AppState{ gameState, uiState = UIState{ camera }} = do
  allEvents <- use #events
  modify' (set #events [])
  UI.pushRenderStack
  UI.render (SystemMap.Rendering.render camera gameState)
  pure (foldMap (Endo . handleEvent) allEvents)

handleEvent :: SDL.Event -> (AppState -> AppState)
handleEvent = \case
  MousePressEvent SDL.ButtonLeft _ ->
    set (#uiState . #draggingCamera) True

  MouseReleaseEvent _ ->
    set (#uiState . #draggingCamera) False

  -- TODO do not use MouseMotionEvent
  MouseMotionEvent (fmap fromIntegral -> motionPx) ->
    over #uiState \s@UIState{ camera, draggingCamera } ->
      if draggingCamera
      then
        let motionAu = Camera.screenToVector camera motionPx
        in s{ camera = over #eyeFrom (subtract motionAu) camera }
      else
        s

  MouseWheelEvent (fromIntegral -> amount) -> do
    over #uiState \s@UIState{ camera, draggingCamera } ->
      if draggingCamera
      then s
      else
        let
          scale = view (#scale . _x) camera
          zoomLevel = cbrt scale
          zoomLevel' = clamp 1.5 (zoomLevel + 0.5 * amount) 70
          auInPixels' = zoomLevel' * zoomLevel' * zoomLevel'
          scale' = V2 auInPixels' (- auInPixels')
        in
          s{ camera = set #scale scale' camera }

  _ -> id
