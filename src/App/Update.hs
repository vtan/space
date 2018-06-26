module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Body as Body
import qualified App.Camera as Camera
import qualified App.PlottedPath as PlottedPath
import qualified App.Rect as Rect
import qualified App.Render.Rendering as Rendering
import qualified App.Ship as Ship
import qualified App.Update.Slot as Slot
import qualified App.Update.Widget as Widget
import qualified App.Update.Updating as Updating
import qualified Linear as Lin
import qualified SDL as SDL

import App.Body (Body(..))
import App.GameState (GameState(..))
import App.Ship (Ship(..))
import App.Uid (Uid(..))
import App.Update.Events
import App.Update.Updating (Updating)
import App.Util (clamp, showDate, showDuration)
import Data.String (fromString)

update :: GameState -> Updating GameState
update gs = do
  toggleShips <- Updating.consumeEvents (\case KeyPressEvent SDL.ScancodeS -> True; _ -> False)
    <&> (not . null)
  when toggleShips $ #ui . #shipWindowOpen %= not
  gs' <- handleUI gs
  use #events <&> foldl' handleEvent gs'

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs = \case
  MousePressEvent SDL.ButtonLeft _ ->
    gs
      & #movingViewport .~ True
      & #draggedViewport .~ False
  MousePressEvent SDL.ButtonRight (fmap fromIntegral -> posPx) ->
    let pos = posPx & Camera.screenToPoint (gs ^. #camera)
        shipNo = length (gs ^. #ships)
        shipUid = Uid shipNo
        ship = Ship
          { Ship.uid = shipUid
          , Ship.name = fromString $ "Ship " ++ show shipNo
          , Ship.position = pos
          , Ship.speed = 1 / 1495970 -- 100 km/s
          , Ship.order = Nothing
          }
    in gs & #ships . at shipUid .~ Just ship
  MouseReleaseEvent (fmap fromIntegral -> pos) ->
    gs
      & #movingViewport .~ False
      & (if gs ^. #draggedViewport then id else handleClick pos)
  MouseMotionEvent (fmap fromIntegral -> motionPx) ->
    if gs ^. #movingViewport
    then 
      let motionAu = Camera.screenToVector (gs ^. #camera) motionPx
      in gs
        & #camera . #eyeFrom -~ motionAu
        & #draggedViewport .~ True
    else gs
  MouseWheelEvent (fromIntegral -> amount) ->
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
            orderMay = Ship.MoveToBody <$> pure (body ^. #uid) <*> pathMay
        in gs & #ships . at uid . _Just . #order .~ orderMay
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
        case ship ^. #order of
          Just Ship.MoveToBody{ Ship.path } -> ship
            & #position .~ (path & PlottedPath.atTime time)
            & (if time >= path ^. #endTime then #order .~ Nothing else id)
          Nothing -> ship
      )

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #shipWindowOpen) >>= \case
    True -> do
      selectedShipUid <- Slot.use $ #ui . #selectedShipUid
      ifor_ (gs ^.. #ships . folded) $ \i Ship{ Ship.uid, Ship.name } -> do
        let rect = Rect.fromMinSize (V2 64 (64 + fromIntegral i * 16)) (V2 256 16)
        selected <- Widget.selectable (elem uid selectedShipUid) rect name
        when selected $ Slot.assign (#ui . #selectedShipUid) (Just uid)
      let selectedShip = selectedShipUid >>= (\uid -> gs ^. #ships . at uid)
      for_ selectedShip $ \Ship{ Ship.name, Ship.speed, Ship.order } ->
        Updating.renderUI $ do
          Rendering.text (V2 (64 + 256 + 16) 64) name
          Rendering.text (V2 (64 + 256 + 16) (64 + 16)) . fromString $
            printf "Speed: %.0f km/s" (speed * 149597000) -- TODO magic number
          case order of
            Just o ->
              let (orderStr, etaStr) = case o of
                    Ship.MoveToBody{ Ship.bodyUid, Ship.path } ->
                      let bodyName = gs ^? #bodies . at bodyUid . _Just . #name & fromMaybe "???"
                          etaDate = showDate (path ^. #endTime)
                          etaDuration = showDuration (path ^. #endTime - gs ^. #time)
                      in (printf "move to %s" bodyName, printf "%s, %s" etaDate etaDuration)
              in do
                Rendering.text (V2 (64 + 256 + 16) (64 + 32)) . fromString $
                  "Current order: " ++ orderStr
                Rendering.text (V2 (64 + 256 + 16) (64 + 48)) . fromString $
                  "ETA: " ++ etaStr
            Nothing ->
              Rendering.text (V2 (64 + 256 + 16) (64 + 32)) $ "No current order"
      pure gs
    False -> pure gs