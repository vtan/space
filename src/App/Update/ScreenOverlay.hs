module App.Update.ScreenOverlay
  ( update )
where

import App.Prelude

import qualified App.Dimension.Time as Time
import qualified App.Update.Logic as Logic
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified SDL

import App.Model.GameState (GameState(..))
import App.Update.Events
import App.Update.Updating (Updating)
import App.Util (whenAlt)
import Data.String (fromString)

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
    then Logic.jumpToNextMidnight gs
    else gs

handleUI :: GameState -> Updating GameState
handleUI gs =
  Updating.childLayout "overlay" $ do
    toggleColonies <- Updating.childBounds "colonies" $ \bounds ->
      Widget.button bounds "Colonies"
        <&> whenAlt (ToggleWindow UIState.ColonyWindow)

    toggleShips <- Updating.childBounds "ships" $ \bounds ->
      Widget.button bounds "Ships"
        <&> whenAlt (ToggleWindow UIState.ShipWindow)

    timeAction <- Updating.childLayout "timePanel" $
      timePanel gs

    activeWindow <- use (#ui . #activeWindow)
    case toggleColonies <|> toggleShips <|> timeAction of
      Just (ToggleWindow w) | w `elem` activeWindow ->
        gs <$ (#ui . #activeWindow .= Nothing)
      Just (ToggleWindow w) ->
        gs <$ (#ui . #activeWindow .= Just w)
      Just NextMidnight ->
        pure $ Logic.jumpToNextMidnight gs
      Just (SetSpeed speed) ->
        gs <$ setGameSpeed speed
      Nothing -> pure gs

timePanel :: GameState -> Updating (Maybe Action)
timePanel gs = do
  screenWidth <- use (#ui . #camera . #eyeTo . _x)
    <&> ((* 2) >>> floor)
  groupBounds <- view (#widgetTree . #bounds)
  let controlsOffset = screenWidth - 2 * (groupBounds ^. #xy . _x) - (groupBounds ^. #wh . _x)

  nextMidnight <- Updating.childBounds "nextMidnight" $ \bounds ->
    Widget.button (bounds & #xy . _x +~ controlsOffset) "next"
      <&> whenAlt NextMidnight

  setSpeed <- (["stop", "1min", "10min", "1h", "12h", "5d"] :: [Text])
    & itraverse (\speed label -> do
      let widget = fromString ("speed" ++ show speed)
      Updating.childBounds widget $ \bounds ->
        Widget.button (bounds & #xy . _x +~ controlsOffset) label
          <&> whenAlt (SetSpeed speed)
    )
    & fmap asum

  Updating.childBounds "currentTime" $ \bounds -> do
    let offset = screenWidth - 2 * (bounds ^. #xy . _x) - (bounds ^. #wh . _x)
    Widget.label (bounds ^. #xy & _x +~ offset) (gs ^. #time & Time.printDate & fromString)

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
