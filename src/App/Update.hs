module App.Update
  ( update )
where

import App.Prelude

import qualified App.Update.ColonyWindow as ColonyWindow
import qualified App.Update.Logic as Logic
import qualified App.Update.ShipWindow as ShipWindow
import qualified App.Update.SystemMap as SystemMap
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified SDL

import App.Model.GameState (GameState(..))
import App.Update.Events
import App.Update.Updating (Updating)

update :: GameState -> Updating GameState
update gs = do
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

  gs' <- handleUI gs
  gs'' <- SystemMap.update gs'

  pure $ case gs ^. #timeStepPerFrame of
    Just step -> gs'' & Logic.stepTime step
    Nothing -> gs''

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #activeWindow) >>= \case
    Just UIState.ColonyWindow -> ColonyWindow.update gs
    Just UIState.ShipWindow -> ShipWindow.update gs
    Nothing -> pure gs
