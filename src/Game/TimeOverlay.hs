module Game.TimeOverlay
  ( timeOverlay )
where

import GlobalImports

import qualified App.Logic.TimeStep as TimeStepLogic
import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widgets as Widgets

import Game.Common.Display (display)
import Core.Common.Rect (Rect(..))
import Game.Dimension.Time (Time)
import App.Model.GameState (GameState(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent, UIContext(..))
import Game.AppState (AppState(..))

timeOverlay :: AppState -> UIComponent AppState
timeOverlay AppState{ gameState = GameState{ time } } =
  let
    size = V2 400 100
    nextButton = Widgets.button "next" (over #gameState TimeStepLogic.jumpToNextMidnight)
    timeButtons = timeChoices & map \(label, choice) ->
      Widgets.button label (set #timeStep choice)
  in do
    UIContext{ scaledScreenSize } <- ask
    let
      V2 x _ = scaledScreenSize - size - 4
      position = V2 x 4
    UI.cursorAt (Rect position size) $
      Layout.vertical
        [ DefaultSized $ Layout.horizontal (map Stretched (nextButton : timeButtons))
        , DefaultSized $ Layout.horizontal
            [ Stretched UI.empty
            , Sized 150 $ Widgets.label (display time)
            ]
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
