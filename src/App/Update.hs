module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Ship as Ship
import qualified App.Update.Logic as Logic
import qualified App.Update.Widget as Widget
import qualified App.Update.Updating as Updating
import qualified SDL as SDL

import App.GameState (GameState(..))
import App.Rect (Rect(..))
import App.Ship (Ship(..))
import App.Update.Events
import App.Update.Updating (Updating)
import App.Util (clamp, showDate, showDuration)
import Data.String (fromString)

update :: GameState -> Updating GameState
update gs = do
  toggleShips <- Updating.consumeEvents (\case KeyPressEvent SDL.ScancodeS -> Just (); _ -> Nothing)
    <&> (not . null)
  when toggleShips $ #ui . #shipWindowOpen %= not
  gs' <- handleUI gs
  gs'' <- use #events <&> foldl' handleEvent gs'
  pure $ case gs ^. #timeStepPerFrame of
    Just step -> gs'' & Logic.stepTime step
    Nothing -> gs''

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs = \case
  MousePressEvent SDL.ButtonLeft _ ->
    gs & #movingViewport .~ True
  MousePressEvent SDL.ButtonRight (fmap fromIntegral -> posPx) ->
    let pos = posPx & Camera.screenToPoint (gs ^. #camera)
    in gs & Logic.addShip pos
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
  KeyPressEvent SDL.ScancodePeriod | gs ^. #timeStepPerFrame & has _Nothing -> 
    let now = gs ^. #time
        nextMidnight = (quot (gs ^. #time) 86400 + 1) * 86400
    in gs & Logic.stepTime (nextMidnight - now)
  KeyPressEvent SDL.ScancodeGrave -> gs & #timeStepPerFrame .~ Nothing
  KeyPressEvent SDL.Scancode1 -> gs & #timeStepPerFrame .~ Just 1
  KeyPressEvent SDL.Scancode2 -> gs & #timeStepPerFrame .~ Just 10
  KeyPressEvent SDL.Scancode3 -> gs & #timeStepPerFrame .~ Just 100
  KeyPressEvent SDL.Scancode4 -> gs & #timeStepPerFrame .~ Just 1200
  KeyPressEvent SDL.Scancode5 -> gs & #timeStepPerFrame .~ Just (3 * 3600)
  _ -> gs

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

        moveTo <- Widget.button (Rect (p + V2 (128 + 4) 64) (V2 56 12)) "Move to..."
        cancel <- Widget.button (Rect (p + V2 (128 + 4 + 56 + 4) 64) (V2 56 12)) "Cancel"

        selectedBody <- use (#ui . #selectedBodyUid) >>= Widget.listBox
          (Rect (p + V2 (128 + 4) 80) (V2 128 176)) 16
          (view #uid) (view #name)
          (gs ^.. #bodies . folded)
        #ui . #selectedBodyUid .= selectedBody ^? _Just . #uid
        
        let gs' 
              | moveTo = Logic.moveShipToBody <$> selectedShip <*> selectedBody <*> pure gs
              | cancel = Logic.cancelShipOrder <$> selectedShip <*> pure gs
              | otherwise = Nothing
        pure $ gs' & fromMaybe gs

      let gs' = gsMay' & fromMaybe gs
      pure gs'
    False -> pure gs