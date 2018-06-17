module App.Update 
  ( update )
where

import App.Prelude

import qualified SDL as SDL

import qualified App.Camera as Camera
import App.GameState (GameState(..))
import App.Update.Events
import App.Util (clamp)

update :: Float -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #totalTime (+ lastFrameTime)
  >>> (\gs -> foldl' handleEvent gs events)

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs = \case
  QuitEvent -> 
    gs & #quit .~ True
  MousePressEvent (_ :: V2 Int) ->
    gs & #movingViewport .~ True
  MouseReleaseEvent (_ :: V2 Int) ->
    gs & #movingViewport .~ False
  MouseMotionEvent (motionPx :: V2 Double) ->
    if gs ^. #movingViewport
    then 
      let motionAu = Camera.screenToVector (gs ^. #camera) motionPx
      in gs & #camera . #eyeFrom -~ motionAu
    else gs
  MouseWheelEvent amount ->
    if gs ^. #movingViewport
    then gs
    else
      let zoomLevel = sqrt $ gs ^. #camera . #scale . _x
          zoomLevel' = clamp 1.5 (zoomLevel + 0.5 * amount) 30
          auInPixels' = zoomLevel' * zoomLevel'
          scale' = V2 auInPixels' (- auInPixels')
      in gs & #camera . #scale .~ scale'
  _ -> gs