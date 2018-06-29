module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Body as Body
import qualified App.Camera as Camera
import qualified App.PlottedPath as PlottedPath
import qualified App.Ship as Ship
import qualified App.Update.Widget as Widget
import qualified App.Update.Updating as Updating
import qualified SDL as SDL

import App.Body (Body(..))
import App.GameState (GameState(..))
import App.Rect (Rect(..))
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
    gs & #movingViewport .~ True
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
  _ -> gs

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
      Widget.window (Rect (V2 32 32) (V2 (128 + 256 + 16) (256 + 24))) 16 "Ships"
      let p = V2 36 52

      selectedShip <- use (#ui . #selectedShipUid) >>= Widget.listBox
        (Rect p (V2 128 256)) 16
        (view #uid) (view #name)
        (gs ^.. #ships . folded)
      #ui . #selectedShipUid .= selectedShip ^? _Just . #uid

      gsMay' <- for selectedShip $ \Ship{ Ship.name, Ship.speed, Ship.order } -> do
        let commonLabels = [name, fromString (printf "Speed: %.0f km/s" (speed * 149597000))] -- TODO magic number
            orderLabels = case order of
              Just o ->
                let (orderStr, etaStr) = case o of
                      Ship.MoveToBody{ Ship.bodyUid, Ship.path } ->
                        let bodyName = gs ^? #bodies . at bodyUid . _Just . #name & fromMaybe "???"
                            etaDate = showDate (path ^. #endTime)
                            etaDuration = showDuration (path ^. #endTime - gs ^. #time)
                        in (printf "move to %s" bodyName, printf "%s, %s" etaDate etaDuration)
                in fromString <$> ["Current order: " ++ orderStr, "ETA: " ++ etaStr]
              Nothing -> ["No current order"]
        Widget.labels (p + V2 (128 + 4) 0) 16 (commonLabels ++ orderLabels)

        moveTo <- Widget.button (Rect (p + V2 (128 + 4) 64) (V2 64 12)) "Move to..."

        selectedBody <- use (#ui . #selectedBodyUid) >>= Widget.listBox
          (Rect (p + V2 (128 + 4) 80) (V2 128 176)) 16
          (view #uid) (view #name)
          (gs ^.. #bodies . folded)
        #ui . #selectedBodyUid .= selectedBody ^? _Just . #uid
        
        pure $ if
          | moveTo ->
              moveShipToBody <$> selectedShip <*> selectedBody <*> pure gs
              & fromMaybe gs
          | otherwise -> gs

      let gs' = gsMay' & fromMaybe gs
      pure gs'
    False -> pure gs

moveShipToBody :: Ship -> Body -> GameState -> GameState
moveShipToBody Ship{ Ship.uid, Ship.position, speed } body gs =
  let pathMay = PlottedPath.plot (gs ^. #time) position speed body
      orderMay = Ship.MoveToBody <$> pure (body ^. #uid) <*> pathMay
  in gs & #ships . at uid . _Just . #order .~ orderMay