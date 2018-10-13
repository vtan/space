module App.Logic.Ship where

import App.Prelude

import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Ship as Ship

import App.Common.Uid (Uid(..))
import App.Model.Body (Body(..))
import App.Model.GameState (GameState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))

moveShipToBody :: Ship -> Uid Body -> GameState -> GameState
moveShipToBody Ship{ Ship.uid, Ship.position, speed } bodyUid gs =
  let pathMay = PlottedPath.plot (gs ^. #time) position speed bodyUid (gs ^. #rootBody)
      orderMay = Ship.MoveToBody <$> pure bodyUid <*> pathMay
  in gs & #ships . at uid . _Just . #order .~ orderMay

cancelShipOrder :: Ship -> GameState -> GameState
cancelShipOrder Ship{ Ship.uid } gs =
  gs & #ships . at uid . _Just . #order .~ Nothing

loadResourceToShip :: Maybe Int -> Resource -> Ship -> GameState -> GameState
loadResourceToShip qtyOrAll resource Ship{ Ship.uid = shipUid, attachedToBody, cargoCapacity, loadedCargo } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    availableOnColony <- gs ^? #colonies . at bodyUid . _Just . #stockpile . at resource . _Just
    let remainingCargoSpace = cargoCapacity - sum loadedCargo
    let loadedQty = minimum (toList qtyOrAll ++ [availableOnColony, remainingCargoSpace])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedCargo . at resource . non 0 +~ loadedQty
      & #colonies . at bodyUid . _Just . #stockpile . at resource . non 0 -~ loadedQty

unloadResourceFromShip :: Maybe Int -> Resource -> Ship -> GameState -> GameState
unloadResourceFromShip qtyOrAll resource Ship{ Ship.uid = shipUid, attachedToBody, loadedCargo } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    _ <- gs ^. #colonies . at bodyUid
    availableOnShip <- loadedCargo ^. at resource
    let unloadedQty = minimum (toList qtyOrAll ++ [availableOnShip])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedCargo . at resource . non 0 -~ unloadedQty
      & #colonies . at bodyUid . _Just . #stockpile . at resource . non 0 +~ unloadedQty

loadPopulationToShip :: Maybe Int -> Ship -> GameState -> GameState
loadPopulationToShip qtyOrAll Ship{ Ship.uid = shipUid, attachedToBody, cabinCapacity, loadedPopulation } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    populationOnColony <- gs ^? #colonies . at bodyUid . _Just . #population
    let remainingCabinSpace = cabinCapacity - loadedPopulation
    let loadedQty = minimum (toList qtyOrAll ++ [populationOnColony, remainingCabinSpace])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedPopulation +~ loadedQty
      & #colonies . at bodyUid . _Just . #population -~ loadedQty

unloadPopulationFromShip :: Maybe Int -> Ship -> GameState -> GameState
unloadPopulationFromShip qtyOrAll Ship{ Ship.uid = shipUid, attachedToBody, loadedPopulation } gs =
  fromMaybe gs $ do
    bodyUid <- attachedToBody
    _ <- gs ^. #colonies . at bodyUid
    let unloadedQty = minimum (toList qtyOrAll ++ [loadedPopulation])
    pure $ gs
      & #ships . at shipUid . _Just . #loadedPopulation -~ unloadedQty
      & #colonies . at bodyUid . _Just . #population +~ unloadedQty
