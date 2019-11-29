module Game.Overlay.TimeOverlay
  ( timeOverlay )
where

import GlobalImports

import qualified Core.UI.Layout as Layout
import qualified Core.UI.UI as UI
import qualified Core.UI.Widget as Widget
import qualified Game.TimeLogic as TimeLogic

import Core.Common.Rect (Rect(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.UI (UIComponent, UIContext(..))
import Game.AppState (AppState(..))
import Game.GameState (GameState(..))
import Game.Common.Display (display)
import Game.Dimension.Time (Time)

timeOverlay :: AppState -> UIComponent AppState
timeOverlay AppState{ gameState = GameState{ time } } =
  let
    size = V2 400 100
    nextButton = Widget.button "next" (over #gameState TimeLogic.jumpToNextMidnight)
    timeButtons = timeChoices & map \(label, choice) ->
      Widget.button (display label) (set #timeStep choice)
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
            , Sized 150 $ Widget.label (display time)
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
