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
import App.PlottedPath (PlottedPath(..))
import App.Ship (Ship(..))
import App.Uid (Uid(..))
import App.Update.Events
import App.Util (clamp)
import Data.String (fromString)

update :: Float -> [SDL.Event] -> GameState -> GameState
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
      (Just body, Just ship) ->
        let path = plotPath gs ship body
        in gs & #ships . at (ship ^. #uid) . _Just . #path .~ Just path
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
    & #bodies . traversed %~ Body.move (fromIntegral dt)
    & #ships . traversed %~ (\ship ->
        case ship ^. #path of
          Just path -> ship
            & #position .~ (path & PlottedPath.atTime time)
            & (if time >= path ^. #endTime then #path .~ Nothing else id)
          Nothing -> ship
      )

-- TODO horrible
-- TODO add check against divergence
plotPath :: GameState -> Ship -> Body -> PlottedPath
plotPath gs ship body =
  let waypointDistance = PlottedPath.waypointTime * ship ^. #speed
      waypointDistSq = waypointDistance * waypointDistance
      initialWaypoint = PlottedPath.Waypoint { PlottedPath.time = gs ^. #time, PlottedPath.position = ship ^. #position}
      furtherWaypoints = flip fix (gs ^. #time, ship ^. #position, body) $ \nextWaypoints (time, shipPos, body) ->
        let bodyPos = body ^. #position
            distSq = Lin.qd shipPos bodyPos
        in if distSq <= waypointDistSq
        then
          let ratio = sqrt $ distSq / waypointDistSq
              finalTime = fromIntegral time + ratio * PlottedPath.waypointTime
              finalPos = bodyPos
              waypoint = PlottedPath.Waypoint { PlottedPath.time = round finalTime, PlottedPath.position = finalPos }
          in [waypoint]
        else
          let time' = time + PlottedPath.waypointTime
              shipPos' = shipPos + waypointDistance *^ Lin.normalize (bodyPos - shipPos)
              body' = Body.move PlottedPath.waypointTime body
              waypoint = PlottedPath.Waypoint { PlottedPath.time = time', PlottedPath.position = shipPos' }
          in waypoint : nextWaypoints (time', shipPos', body')
  in PlottedPath.fromWaypoints (initialWaypoint :| furtherWaypoints)