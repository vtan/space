module App.Update.SystemMap
  ( update )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Render.SystemMap as Render.SystemMap
import qualified App.Update.Logic as Logic
import qualified SDL

import App.Model.GameState (GameState(..))
import App.Update.Events
import App.Update.Updating (Updating)
import App.Util (clamp)
import Numeric.Extras (cbrt)

update :: GameState -> Updating GameState
update gs = do
  gs' <- use #events <&> foldl' handleEvent gs
  #deferredRendering %= (Render.SystemMap.render gs' :)
  pure gs'

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs = \case
  MousePressEvent SDL.ButtonLeft _ ->
    gs & #movingViewport .~ True
  MouseReleaseEvent _ ->
    gs & #movingViewport .~ False
  MouseMotionEvent (fmap fromIntegral -> motionPx) ->
    if gs ^. #movingViewport
    then
      let motionAu = Camera.screenToVector (gs ^. #camera) motionPx
      in gs & #camera . #eyeFrom -~ motionAu
    else gs
  MouseWheelEvent (fromIntegral -> amount) ->
    if gs ^. #movingViewport
    then gs
    else
      let zoomLevel = cbrt $ gs ^. #camera . #scale . _x
          zoomLevel' = clamp 1.5 (zoomLevel + 0.5 * amount) 70
          auInPixels' = zoomLevel' * zoomLevel' * zoomLevel'
          scale' = V2 auInPixels' (- auInPixels')
      in gs & #camera . #scale .~ scale'
  KeyPressEvent SDL.ScancodePeriod | gs ^. #timeStepPerFrame & has _Nothing ->
    let now = gs ^. #time
    in gs & Logic.stepTime (Logic.timeUntilNextMidnight now)
  KeyPressEvent SDL.ScancodeGrave -> gs & #timeStepPerFrame .~ Nothing
  KeyPressEvent SDL.Scancode1 -> gs & #timeStepPerFrame .~ Just 1
  KeyPressEvent SDL.Scancode2 -> gs & #timeStepPerFrame .~ Just 10
  KeyPressEvent SDL.Scancode3 -> gs & #timeStepPerFrame .~ Just 100
  KeyPressEvent SDL.Scancode4 -> gs & #timeStepPerFrame .~ Just 1200
  KeyPressEvent SDL.Scancode5 -> gs & #timeStepPerFrame .~ Just (3 * 3600)
  _ -> gs
