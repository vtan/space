module App.UI.ShipWindow
  ( update )
where

import App.Prelude

import qualified App.Dimension.Speed as Speed
import qualified App.Dimension.Time as Time
import qualified App.Logic.Ship as Logic.Ship
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified Data.Text as Text
import qualified Linear as Lin

import App.Common.Uid (Uid)
import App.Common.Util (whenAlt)
import App.Model.Body (Body)
import App.Model.GameState (GameState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Update.Updating (Updating)
import Data.String (fromString)
import Text.Read (readMaybe)

data Action
  = Rename Text
  | MoveToBody (Uid Body)
  | CancelOrder
  | LoadResource Resource (Maybe Int)
  | UnloadResource Resource (Maybe Int)
  | LoadPopulation (Maybe Int)
  | UnloadPopulation (Maybe Int)

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
        cabinAction <- Updating.childLayout "cabinPanel" (cabinPanel ship)

        let moveToSelected = (moveTo *> selectedBody) <&> \b -> MoveToBody (b ^. #uid)
            action = rename <|> moveToSelected <|> cancel <|> cargoAction <|> cabinAction
        pure $ case action of
          Just (Rename name) ->
            gs & #ships . at uid . _Just . #name .~ name
          Just (MoveToBody bodyUid) ->
            Logic.Ship.moveShipToBody ship bodyUid gs
          Just CancelOrder ->
            Logic.Ship.cancelShipOrder ship gs
          Just (LoadResource resource qtyOrAll) ->
            Logic.Ship.loadResourceToShip qtyOrAll resource ship gs
          Just (UnloadResource resource qtyOrAll) ->
            Logic.Ship.unloadResourceFromShip qtyOrAll resource ship gs
          Just (LoadPopulation qtyOrAll) ->
            Logic.Ship.loadPopulationToShip qtyOrAll ship gs
          Just (UnloadPopulation qtyOrAll) ->
            Logic.Ship.unloadPopulationFromShip qtyOrAll ship gs
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
  let commonLabels = [fromString (printf "Speed: %s" (Speed.printKmPerSec speed))]
      orderLabels = case order of
        Just o ->
          let (orderStr, etaStr) = case o of
                Ship.MoveToBody{ Ship.bodyUid, Ship.path } ->
                  let bodyName = gs ^? #bodies . at bodyUid . _Just . #name & fromMaybe "???"
                      etaDate = Time.printDate (path ^. #endTime)
                      etaDuration = Time.printDuration (path ^. #endTime - gs ^. #time)
                      actualSpeed = Speed.div
                        (Lin.distance (path ^. #endPos) (path ^. #startPos))
                        (path ^. #endTime - path ^. #startTime)
                  in (printf "move to %s (act. spd. %s)" bodyName (Speed.printKmPerSec actualSpeed), printf "%s, %s" etaDate etaDuration)
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
      Resource.all

  Updating.childBounds "qtyLabel" $ \bounds ->
    Widget.label (bounds ^. #xy) "Qty:"

  qty <- Updating.childBounds "qty" $ \bounds ->
    Widget.textBox "cargoQty" bounds #editedResourceQty
      <&> (Text.unpack >>> readMaybe @Int >>> mfilter (>= 0))

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
      fromString (printf "Cargo capacity: %d t" cargoCapacity)

  Updating.childBounds "loadedCargo" $ \bounds ->
    Widget.labels (bounds ^. #xy) 20 $
      loadedCargo
        & itoList
        & map (\(resource, loadedQty) ->
          fromString (printf "Stored %s: %d t" (show resource) loadedQty)
        )

  pure $ load <|> loadAll <|> unload <|> unloadAll

cabinPanel :: Ship -> Updating (Maybe Action)
cabinPanel Ship{ cabinCapacity, loadedPopulation } = do
  Updating.childBounds "cabinCapacity" $ \bounds ->
    Widget.label (bounds ^. #xy) $
      fromString (printf "Cabin capacity: %d / %d ppl" loadedPopulation cabinCapacity)

  Updating.childBounds "popLabel" $ \bounds ->
    Widget.label (bounds ^. #xy) "Pop:"

  qty <- Updating.childBounds "pop" $ \bounds ->
    Widget.textBox "popQty" bounds #editedPopulationQty
      <&> (Text.unpack >>> readMaybe @Int >>> mfilter (>= 0))

  let (loadLabel, unloadLabel) = case qty of
        Just _ -> ("Load", "Unload")
        Nothing -> ("Load max", "Unload max")

  load <- Updating.childBounds "load" $ \bounds ->
    Widget.button bounds loadLabel
      <&> whenAlt (LoadPopulation qty)

  unload <- Updating.childBounds "unload" $ \bounds ->
    Widget.button bounds unloadLabel
      <&> whenAlt (UnloadPopulation qty)

  let action = load <|> unload
  when (action & has _Just) $
    #ui . #editedPopulationQty .= ""

  pure action
