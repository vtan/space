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
  KeyPressEvent SDL.ScancodePeriod -> Logic.jumpToNextMidnight gs
  KeyPressEvent SDL.ScancodeGrave -> Logic.setGameSpeed 0 gs
  KeyPressEvent SDL.Scancode1 -> Logic.setGameSpeed 1 gs
  KeyPressEvent SDL.Scancode2 -> Logic.setGameSpeed 2 gs
  KeyPressEvent SDL.Scancode3 -> Logic.setGameSpeed 3 gs
  KeyPressEvent SDL.Scancode4 -> Logic.setGameSpeed 4 gs
  KeyPressEvent SDL.Scancode5 -> Logic.setGameSpeed 5 gs
  _ -> gs
