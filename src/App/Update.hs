module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Body as Body
import qualified App.Camera as Camera
import qualified App.Ship as Ship
import qualified Linear as Lin
import qualified SDL as SDL

import App.Body (Body(..))
import App.GameState (GameState(..))
import App.Ship (Ship(..))
import App.Uid (Uid(..))
import App.Update.Events
import App.Util (clamp)
import Data.String (fromString)

update :: Float -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #totalTime (+ lastFrameTime)
  >>> (\gs -> foldl' handleEvent gs events)

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs = \case
  QuitEvent -> 
    gs & #quit .~ True
  MousePressEvent SDL.ButtonLeft (_ :: V2 Int) ->
    gs
      & #movingViewport .~ True
      & #draggedViewport .~ False
  MousePressEvent SDL.ButtonRight posPx ->
    let pos = posPx & Camera.screenToPoint (gs ^. #camera)
        shipNo = length (gs ^. #ships)
        shipUid = Uid shipNo
        ship = Ship
          { Ship.uid = shipUid
          , Ship.name = fromString $ "Ship " ++ show shipNo
          , Ship.position = pos
          }
    in gs & #ships . at shipUid .~ Just ship
  MouseReleaseEvent pos ->
    gs
      & #movingViewport .~ False
      & (if gs ^. #draggedViewport then id else handleClick pos)
  MouseMotionEvent (motionPx :: V2 Double) ->
    if gs ^. #movingViewport
    then 
      let motionAu = Camera.screenToVector (gs ^. #camera) motionPx
      in gs
        & #camera . #eyeFrom -~ motionAu
        & #draggedViewport .~ True
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

handleClick :: V2 Int -> GameState -> GameState
handleClick clickPosPx gs =
  let camera = gs ^. #camera
      clickPos = clickPosPx & fmap fromIntegral & Camera.screenToPoint camera
      radius = Body.drawnRadius & Camera.unscale camera
      rsq = radius * radius
      clickedBody = gs ^. #bodies & find (\Body{ Body.position } ->
          Lin.qd position clickPos <= rsq
        )
      clickedShip = gs ^. #ships & find (\Ship{ Ship.position } ->
          Lin.qd position clickPos <= rsq
        )
  in case clickedBody of
    Just Body{ Body.uid } -> gs & #selectedBodyUid .~ Just uid
    Nothing -> 
      case clickedShip of
        Just Ship{ Ship.uid } -> gs & #selectedShipUid .~ Just uid
        Nothing -> gs
          & #selectedBodyUid .~ Nothing
          & #selectedShipUid .~ Nothing