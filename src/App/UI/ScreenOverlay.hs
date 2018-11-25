module App.UI.ScreenOverlay
  ( update )
where

import App.Prelude

import qualified App.Common.HashedText as HashedText
import qualified App.Common.Print as Print
import qualified App.Dimension.Time as Time
import qualified App.Logic.TimeStep as Logic.TimeStep
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified SDL

import App.Common.Util (boolToMaybe)
import App.Model.GameState (GameState(..))
import App.Update.Events
import App.Update.Updating (Updating)

data Action
  = ToggleWindow UIState.Window
  | NextMidnight
  | SetSpeed Int

update :: GameState -> Updating GameState
update =
  handleKeyEvents >=> handleUI

handleKeyEvents :: GameState -> Updating GameState
handleKeyEvents gs = do
  newGameSpeed <-
    Updating.consumeEvents (\case
      KeyPressEvent SDL.ScancodeGrave -> Just 0
      KeyPressEvent SDL.Scancode1 -> Just 1
      KeyPressEvent SDL.Scancode2 -> Just 2
      KeyPressEvent SDL.Scancode3 -> Just 3
      KeyPressEvent SDL.Scancode4 -> Just 4
      KeyPressEvent SDL.Scancode5 -> Just 5
      _ -> Nothing
    ) <&> listToMaybe

  nextMidnight <-
    Updating.consumeEvents (\case
      KeyPressEvent SDL.ScancodePeriod -> Just ()
      _ -> Nothing
    ) <&> (not . null)

  for_ newGameSpeed setGameSpeed
  pure $ if nextMidnight
    then Logic.TimeStep.jumpToNextMidnight gs
    else gs

handleUI :: GameState -> Updating GameState
handleUI gs =
  Updating.useWidget "overlay" $ do
    toggleColonies <- Updating.widget "colonies"
      (Widget.button "Colonies")
      <&> boolToMaybe (ToggleWindow UIState.ColonyWindow)

    toggleShips <- Updating.widget "ships"
      (Widget.button "Ships")
      <&> boolToMaybe (ToggleWindow UIState.ShipWindow)

    toggleProduction <- Updating.widget "production"
      (Widget.button "Production")
      <&> boolToMaybe (ToggleWindow UIState.ProductionWindow)

    timeAction <- Updating.useWidget "timePanel" $
      timePanel gs

    activeWindow <- use (#ui . #activeWindow)
    case toggleColonies <|> toggleShips <|> toggleProduction <|> timeAction of
      Just (ToggleWindow w) | w `elem` activeWindow ->
        gs <$ (#ui . #activeWindow .= Nothing)
      Just (ToggleWindow w) ->
        gs <$ (#ui . #activeWindow .= Just w)
      Just NextMidnight ->
        pure $ Logic.TimeStep.jumpToNextMidnight gs
      Just (SetSpeed speed) ->
        gs <$ setGameSpeed speed
      Nothing -> pure gs

timePanel :: GameState -> Updating (Maybe Action)
timePanel gs = do
  screenWidth <- use (#ui . #camera . #eyeTo . _x)
    <&> ((* 2) >>> floor)
  groupBounds <- view (#widgetTree . #bounds)
  let controlsOffset = screenWidth - 2 * (groupBounds ^. #xy . _x) - (groupBounds ^. #wh . _x)

  nextMidnight <- Updating.widget "nextMidnight"
    ( #bounds . #xy . _x +~ controlsOffset
      >>> Widget.button "next"
    ) <&> boolToMaybe NextMidnight

  setSpeed <- (["stop", "1min", "10min", "1h", "12h", "5d"] :: [Text])
    & itraverse (\speed label -> do
      let widget = HashedText.new (Print.toText ("speed" <> Print.int speed))
      Updating.widget widget
        ( #bounds . #xy . _x +~ controlsOffset
          >>> Widget.button label
        ) <&> boolToMaybe (SetSpeed speed)
    )
    & fmap asum

  Updating.widget "currentTime"
    ( ( #bounds %~ \bounds ->
          let offset = screenWidth - 2 * (bounds ^. #xy . _x) - (bounds ^. #wh . _x)
          in bounds & #xy . _x +~ offset
      )
      >>> Widget.label (gs ^. #time & Time.printDateTime)
    )

  pure (nextMidnight <|> setSpeed)

setGameSpeed :: Int -> Updating ()
setGameSpeed speed =
  -- TODO assuming 60 fps
  case speed of
    0 -> #timeStepPerFrame .= Nothing
    1 -> #timeStepPerFrame .= Just 1
    2 -> #timeStepPerFrame .= Just 10
    3 -> #timeStepPerFrame .= Just 60
    4 -> #timeStepPerFrame .= Just (12 * 60)
    5 -> #timeStepPerFrame .= Just (5 * 24 * 60)
    _ -> pure ()
