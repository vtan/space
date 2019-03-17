module App.Update.ScreenOverlay
  ( update )
where

import App.Prelude

import qualified App.Dimension.Time as Time
import qualified App.Logic.TimeStep as Logic.TimeStep
import qualified App.Update.UIState as UIState
import qualified App.UIBuilder.UIBuilder as UI
import qualified App.UIBuilder.Widget as Widget
import qualified SDL

import App.Common.EventPatterns
import App.Common.Util (clamp)
import App.Model.GameState (GameState(..))
import App.Update.Updating (Updating)

data Action
  = ToggleWindow UIState.Window
  | NextMidnight
  | SetSpeed Int
  | SetScaleFactor Int

update :: GameState -> Updating GameState
update =
  handleKeyEvents >=> handleUI

handleKeyEvents :: GameState -> Updating GameState
handleKeyEvents gs = do
  (fmap getFirst -> newGameSpeed) <- UI.consumeEvents $ \case
    KeyPressEvent SDL.ScancodeGrave -> Just (First 0)
    KeyPressEvent SDL.Scancode1 -> Just (First 1)
    KeyPressEvent SDL.Scancode2 -> Just (First 2)
    KeyPressEvent SDL.Scancode3 -> Just (First 3)
    KeyPressEvent SDL.Scancode4 -> Just (First 4)
    KeyPressEvent SDL.Scancode5 -> Just (First 5)
    _ -> Nothing

  Any nextMidnight <- UI.consumeEvents $ \case
    KeyPressEvent SDL.ScancodePeriod -> Any True
    _ -> mempty

  for_ newGameSpeed setGameSpeed
  pure $ if nextMidnight
    then Logic.TimeStep.jumpToNextMidnight gs
    else gs

handleUI :: GameState -> Updating GameState
handleUI gs = do
  topGroupAct <- topGroup
  timeControlGroupAct <- timeControlGroup
  scalingGroupAct <- scalingGroup
  currentTimeGroup gs

  activeWindow <- use (#ui . #activeWindow)
  case topGroupAct <|> timeControlGroupAct <|> scalingGroupAct of
    Just (ToggleWindow w) | w `elem` activeWindow ->
      gs <$ (#ui . #activeWindow .= Nothing)
    Just (ToggleWindow w) ->
      gs <$ (#ui . #activeWindow .= Just w)
    Just NextMidnight ->
      pure $ Logic.TimeStep.jumpToNextMidnight gs
    Just (SetSpeed speed) ->
      gs <$ setGameSpeed speed
    Just (SetScaleFactor scaleFactor) ->
      gs <$ (#newScaleFactor .= Just scaleFactor)
    Nothing -> pure gs

topGroup :: Updating (Maybe Action)
topGroup =
  UI.group UI.Horizontal $
    UI.positioned 1 . UI.sized (V2 22 5) $ do
      colonies <- Widget.button "Colonies"
      pure $ if
        | colonies -> Just (ToggleWindow UIState.ColonyWindow)
        | otherwise -> Nothing

timeControlGroup :: Updating (Maybe Action)
timeControlGroup =
  let
    labels = ["stop", "1min", "10min", "1h", "12h", "5d"] :: [Text]
    labelCount = length labels + 1
    buttonWidth = 12
    padding = 1
    groupWidth = labelCount * (buttonWidth + padding)
  in do
    x <- UI.startOfRightAligned groupWidth
    UI.group UI.Horizontal $
      UI.positioned (V2 x 1) . UI.sized (V2 buttonWidth 5) $ do
        nextMidnight <- Widget.button "next" <&> \case
          True -> Just NextMidnight
          False -> Nothing

        speeds <- ifor labels $ \speed label -> do
          btn <- Widget.button label
          pure $ if btn
            then Just speed
            else Nothing
        let speed = listToMaybe (catMaybes speeds)
        pure (nextMidnight <|> (SetSpeed <$> speed))

currentTimeGroup :: GameState -> Updating ()
currentTimeGroup GameState{ time } =
  let
    width = 40
    currentTime = Time.printDateTime time
  in do
    x <- UI.startOfRightAligned width
    UI.group UI.Horizontal $
      UI.positioned (V2 x 6) . UI.sized (V2 width 5) $
        Widget.label currentTime

scalingGroup :: Updating (Maybe Action)
scalingGroup = do
  x <- UI.startOfRightAligned 16
  UI.group UI.Horizontal $
    UI.positioned (V2 x 12) . UI.sized (V2 4 5) $ do
      decrease <- Widget.button "-"
      Widget.label "UI"
      increase <- Widget.button "+"
      let diff = if
            | decrease -> Just (-1)
            | increase -> Just 1
            | otherwise -> Nothing
      action <- for diff $ \n -> do
        current <- view (#uiBuilderContext . #scaleFactor)
        let new = clamp 2 (current + n) 6
        pure $
          if new /= current
          then Just (SetScaleFactor new)
          else Nothing
      pure (join action)

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
