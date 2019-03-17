module App.Update.SystemMap
  ( update )
where

import App.Prelude

import qualified App.Common.Camera as Camera
import qualified App.Render.SystemMap as Render.SystemMap
import qualified App.UIBuilder.UIBuilder as UI
import qualified SDL

import App.Common.Util (clamp)
import App.Model.GameState (GameState(..))
import App.Update.Events
import App.Update.Updating (Updating)
import Numeric.Extras (cbrt)

update :: GameState -> Updating GameState
update gs =
  gs <$ do
    use (#uiBuilderState . #events) >>= traverse_ handleEvent
    camera <- use (#ui . #camera)
    UI.pushRender (Render.SystemMap.render camera gs)

handleEvent :: SDL.Event -> Updating ()
handleEvent = \case
  MousePressEvent SDL.ButtonLeft _ ->
    #movingViewport .= True
  MouseReleaseEvent _ ->
    #movingViewport .= False
  MouseMotionEvent (fmap fromIntegral -> motionPx) -> do
    movingViewport <- use #movingViewport
    when movingViewport $ do
      camera <- use (#ui . #camera)
      let motionAu = Camera.screenToVector camera motionPx
      #ui . #camera . #eyeFrom -= motionAu
  MouseWheelEvent (fromIntegral -> amount) -> do
    movingViewport <- use #movingViewport
    unless movingViewport $ do
      scale <- use (#ui . #camera . #scale . _x)
      let zoomLevel = cbrt scale
          zoomLevel' = clamp 1.5 (zoomLevel + 0.5 * amount) 70
          auInPixels' = zoomLevel' * zoomLevel' * zoomLevel'
          scale' = V2 auInPixels' (- auInPixels')
      #ui . #camera . #scale .= scale'
  _ -> pure ()
