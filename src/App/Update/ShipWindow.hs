module App.Update.ShipWindow
  ( update )
where

import App.Prelude

import qualified App.Model.Ship as Ship
import qualified App.Update.Logic as Logic
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified Data.Text as Text
import qualified Linear as Lin

import App.Model.Body (Body)
import App.Model.GameState (GameState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Uid (Uid)
import App.Update.Updating (Updating)
import App.Util (showDate, showDuration, whenAlt)
import Data.String (fromString)
import Text.Read (readMaybe)

data Action
  = Rename Text
  | MoveToBody (Uid Body)
  | CancelOrder
  | LoadResource Resource (Maybe Double)
  | UnloadResource Resource (Maybe Double)

update :: GameState -> Updating GameState
update gs =
  Updating.childLayout "shipWindow" $ do
    Updating.childBounds "decoration" $ \bounds ->
      Widget.window bounds 20 "Ships"

    (selectedShip, clickedShip) <-
      Updating.childBounds "selectedShip" $ \bounds ->
        Widget.listBox
          bounds 20
          (view #uid) (view #name)
          #selectedShip
          (gs ^.. #ships . folded)
    for_ clickedShip $ \ship -> #ui . #editedShipName .= (ship ^. #name)

    gs' <- Updating.childLayout "rightPanel" $
      for selectedShip $ \ship@Ship{ uid } -> do
        rename <- Updating.childLayout "renamePanel" renamePanel
        Updating.childBounds "infoLabels" $ \bounds ->
          infoLabels (bounds ^. #xy) ship gs

        moveTo <- Updating.childBounds "moveTo" $ \bounds ->
          Widget.button bounds "Move to..."
            <&> whenAlt ()
        cancel <- Updating.childBounds "cancel" $ \bounds ->
          Widget.button bounds "Cancel"
            <&> whenAlt CancelOrder

        selectedBody <- Updating.childBounds "selectedBody" $ \bounds ->
          Widget.closedDropdown
            bounds 20 380
            (view #uid) (view #name)
            #selectedBody
            (gs ^.. #bodies . folded)

        cargoAction <- Updating.childLayout "cargoPanel" (cargoPanel ship)

        let moveToSelected = (moveTo *> selectedBody) <&> \b -> MoveToBody (b ^. #uid)
            action = rename <|> moveToSelected <|> cancel <|> cargoAction
        pure $ case action of
          Just (Rename name) ->
            gs & #ships . at uid . _Just . #name .~ name
          Just (MoveToBody bodyUid) ->
            Logic.moveShipToBody ship bodyUid gs
          Just CancelOrder ->
            Logic.cancelShipOrder ship gs
          Just (LoadResource resource qtyOrAll) ->
            Logic.loadResourceToShip qtyOrAll resource ship gs
          Just (UnloadResource resource qtyOrAll) ->
            Logic.unloadResourceFromShip qtyOrAll resource ship gs
          Nothing -> gs

    pure (gs' & fromMaybe gs)

renamePanel :: Updating (Maybe Action)
renamePanel = do
  ename <- Updating.childBounds "shipName" $ \bounds ->
    Widget.textBox "shipName" bounds #editedShipName
  Updating.childBounds "button" $ \bounds ->
    Widget.button bounds "Rename"
      <&> whenAlt (Rename ename)

infoLabels :: V2 Int -> Ship -> GameState -> Updating ()
infoLabels p Ship{ speed, order } gs =
  let commonLabels = [fromString (printf "Speed: %.0f km/s" (speed * 149597000))] -- TODO magic number
      orderLabels = case order of
        Just o ->
          let (orderStr, etaStr) = case o of
                Ship.MoveToBody{ Ship.bodyUid, Ship.path } ->
                  let bodyName = gs ^? #bodies . at bodyUid . _Just . #name & fromMaybe "???"
                      etaDate = showDate (path ^. #endTime)
                      etaDuration = showDuration (path ^. #endTime - gs ^. #time)
                      actualSpeed = Lin.distance (path ^. #endPos) (path ^. #startPos) / fromIntegral (path ^. #endTime - path ^. #startTime) * 149597000
                  in (printf "move to %s (act. spd. %.2f)" bodyName actualSpeed, printf "%s, %s" etaDate etaDuration)
          in fromString <$> ["Current order: " ++ orderStr, "ETA: " ++ etaStr]
        Nothing -> ["No current order"]
  in Widget.labels p 20 (commonLabels ++ orderLabels)

cargoPanel :: Ship -> Updating (Maybe Action)
cargoPanel Ship{ cargoCapacity, loadedCargo } = do
  selectedResource <- Updating.childBounds "selectedResource" $ \bounds ->
    Widget.closedDropdown
      bounds 20 380
      id (show >>> fromString)
      #selectedResource
      [minBound .. maxBound]

  Updating.childBounds "qtyLabel" $ \bounds ->
    Widget.label (bounds ^. #xy) "Qty:"

  qty <- Updating.childBounds "qty" $ \bounds ->
    Widget.textBox "cargoQty" bounds #editedResourceQty
      <&> (Text.unpack >>> readMaybe @Double)

  load <- Updating.childBounds "load" $ \bounds ->
    Widget.button bounds "Load"
      <&> whenAlt (LoadResource <$> selectedResource <*> (fmap Just qty))
      <&> join

  loadAll <- Updating.childBounds "loadAll" $ \bounds ->
    Widget.button bounds "Load all"
      <&> whenAlt (LoadResource <$> selectedResource <*> Just Nothing)
      <&> join

  unload <- Updating.childBounds "unload" $ \bounds ->
    Widget.button bounds "Unload"
      <&> whenAlt (UnloadResource <$> selectedResource <*> (fmap Just qty))
      <&> join

  unloadAll <- Updating.childBounds "unloadAll" $ \bounds ->
    Widget.button bounds "Unload all"
      <&> whenAlt (UnloadResource <$> selectedResource <*> Just Nothing)
      <&> join

  Updating.childBounds "cargoCapacity" $ \bounds ->
    Widget.label (bounds ^. #xy) $
      fromString (printf "Cargo capacity: %.2f t" cargoCapacity)

  Updating.childBounds "loadedCargo" $ \bounds ->
    Widget.labels (bounds ^. #xy) 20 $
      loadedCargo
        & itoList
        & map (\(resource, loadedQty) ->
          fromString (printf "Stored %s: %.2f t" (show resource) loadedQty)
        )

  pure $ load <|> loadAll <|> unload <|> unloadAll
