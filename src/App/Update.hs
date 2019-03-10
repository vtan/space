module App.Update
  ( update )
where

import App.Prelude

import qualified App.Logic.TimeStep as Logic.TimeStep
import qualified App.UI.ColonyWindow as OldColonyWindow
import qualified App.UI.ProductionWindow as ProductionWindow
import qualified App.UI.ShipWindow as ShipWindow
import qualified App.UI.SystemMap as SystemMap
import qualified App.UI2.UI as UI
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified App.View.ColonyWindow as ColonyWindow
import qualified App.View.ScreenOverlay as ScreenOverlay
import qualified SDL

import App.Model.GameState (GameState(..))
import App.Update.Events
import App.Update.Updating (Updating)

update :: GameState -> Updating GameState
update gs = do
  reloadResources <- view (#frameContext . #keyModifier) >>= \case
    (SDL.keyModifierLeftCtrl -> True) ->
      Updating.consumeEvents (\case
        KeyPressEvent SDL.ScancodeR -> Just ()
        _ -> Nothing
      ) <&> (not . null)
    _ -> pure False
  #reloadResources .= reloadResources

  hasFocusedWidget <- use #focusedWidget <&> has _Just
  toggleWindow <- Updating.consumeEvents (\case
      KeyPressEvent SDL.ScancodeC | not hasFocusedWidget -> Just UIState.ColonyWindow
      KeyPressEvent SDL.ScancodeS | not hasFocusedWidget -> Just UIState.ShipWindow
      _ -> Nothing
    ) <&> listToMaybe
  case toggleWindow of
    Just window -> do
      activeWindow <- use (#ui . #activeWindow)
      if elem window activeWindow
      then #ui . #activeWindow .= Nothing
      else #ui . #activeWindow .= Just window
    Nothing -> pure ()

  clickedAnywhere <- Updating.filterEvents (\case MousePressEvent _ _ -> Just (); _ -> Nothing)
    <&> (not . null)
  when clickedAnywhere $ #focusedWidget .= Nothing -- if clicked on a focusable widget, it will consume the click and set the focus

  activeDropdown <- use #activeDropdown
  for_ activeDropdown $ \updateDropdown ->
    updateDropdown *> Updating.pushRendering

  gs' <- gs & (
      fmap UI.group' handleUI
      >=> fmap UI.group' ScreenOverlay.update
      >=> SystemMap.update
    )

  timeStep <- use #timeStepPerFrame
  pure $ case timeStep of
    Just step -> gs' & Logic.TimeStep.stepTime step
    Nothing -> gs'

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #activeWindow) >>= \case
    Just UIState.ColonyWindow -> OldColonyWindow.update gs *> ColonyWindow.update gs
    Just UIState.ShipWindow -> ShipWindow.update gs
    Just UIState.ProductionWindow -> ProductionWindow.update gs
    Nothing -> pure gs
