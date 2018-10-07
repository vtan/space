module App.Update.ScreenOverlay
  ( update )
where

import App.Prelude

import qualified App.Update.Logic as Logic
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget

import App.Model.GameState (GameState(..))
import App.Update.Updating (Updating)
import App.Util (showDate, whenAlt)
import Data.String (fromString)

data Action
  = ToggleWindow UIState.Window
  | NextMidnight
  | SetSpeed Int

update :: GameState -> Updating GameState
update gs =
  Updating.childLayout "overlay" $ do
    toggleColonies <- Updating.childBounds "colonies" $ \bounds ->
      Widget.button bounds "Colonies"
        <&> whenAlt (ToggleWindow UIState.ColonyWindow)

    toggleShips <- Updating.childBounds "ships" $ \bounds ->
      Widget.button bounds "Ships"
        <&> whenAlt (ToggleWindow UIState.ShipWindow)

    nextMidnight <- Updating.childBounds "nextMidnight" $ \bounds ->
      Widget.button bounds "next"
        <&> whenAlt NextMidnight

    setSpeed <- (["stop", "1min", "10min", "1h", "12h", "5d"] :: [Text])
      & itraverse (\speed label -> do
        let widget = fromString ("speed" ++ show speed)
        Updating.childBounds widget $ \bounds ->
          Widget.button bounds label
            <&> whenAlt (SetSpeed speed)
      )
      & fmap asum

    Updating.childBounds "currentTime" $ \bounds ->
      Widget.label (bounds ^. #xy) (gs ^. #time & showDate & fromString)

    activeWindow <- use (#ui . #activeWindow)
    case toggleColonies <|> toggleShips <|> nextMidnight <|> setSpeed of
      Just (ToggleWindow w) | w `elem` activeWindow ->
        gs <$ (#ui . #activeWindow .= Nothing)
      Just (ToggleWindow w) ->
        gs <$ (#ui . #activeWindow .= Just w)
      Just NextMidnight ->
        pure $ Logic.jumpToNextMidnight gs
      Just (SetSpeed speed) ->
        pure $ Logic.setGameSpeed speed gs
      Nothing -> pure gs
