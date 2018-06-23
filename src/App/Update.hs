module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Body as Body
import qualified App.Camera as Camera
import qualified App.PlottedPath as PlottedPath
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

update :: Double -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #totalRealTime (+ lastFrameTime)
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
          , Ship.speed = 1 / 1495970 -- 100 km/s
          , Ship.path = Nothing
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
  KeyPressEvent SDL.Scancode1 -> gs & stepTime 1
  KeyPressEvent SDL.Scancode2 -> gs & stepTime 60
  KeyPressEvent SDL.Scancode3 -> gs & stepTime 600
  KeyPressEvent SDL.Scancode4 -> gs & stepTime 3600
  KeyPressEvent SDL.Scancode5 -> gs & stepTime (3 * 3600)
  KeyPressEvent SDL.Scancode6 -> gs & stepTime (24 * 3600)
  KeyPressEvent SDL.ScancodeP ->
    let bodyMay = gs ^. #selectedBodyUid >>= \uid -> gs ^. #bodies . at uid
        shipMay = gs ^. #selectedShipUid >>= \uid -> gs ^. #ships . at uid
    in case (bodyMay, shipMay) of
      (Just body, Just Ship{ Ship.uid, Ship.position, speed }) ->
        let pathMay = PlottedPath.plot (gs ^. #time) position speed body
        in gs & #ships . at uid . _Just . #path .~ pathMay
      _ -> gs
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

stepTime :: Int -> GameState -> GameState
stepTime dt gs =
  let time = dt + gs ^. #time
  in gs
    & #time .~ time
    & #bodies . traversed %~ Body.atTime time
    & #ships . traversed %~ (\ship ->
        case ship ^. #path of
          Just path -> ship
            & #position .~ (path & PlottedPath.atTime time)
            & (if time >= path ^. #endTime then #path .~ Nothing else id)
          Nothing -> ship
      )