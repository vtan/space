module Game.TimeOverlay
  ( timeOverlay )
where

import App.Prelude

import qualified App.Logic.TimeStep as TimeStepLogic
import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widgets as Widgets

import App.Common.Display (display)
import App.Common.Rect (Rect(..))
import App.Dimension.Time (Time)
import App.Model.GameState (GameState(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))

timeOverlay :: AppState -> UIComponent AppState
timeOverlay AppState{ gameState = GameState{ time } } =
  let
    nextButton = Widgets.button "next" (over #gameState TimeStepLogic.jumpToNextMidnight)
    timeButtons = timeChoices & map \(label, choice) ->
      Widgets.button label (set #timeStep choice)
  in
    UI.cursorAt (Rect (V2 0 0) (V2 400 100)) $
      Layout.vertical
        [ DefaultSized $ Layout.horizontal (map Stretched (nextButton : timeButtons))
        , DefaultSized $ Widgets.label (display time)
        ]

timeChoices :: [(Text, Maybe (Time Int))]
timeChoices =
  [ ("stop", Nothing)
  , ("1min", Just 1)
  , ("10min", Just 10)
  , ("1h", Just 60)
  , ("12h", Just (12 * 60))
  , ("5d", Just (5 * 24 * 60))
  ]
