module Game.Controller where

import App.Prelude

import qualified Core.UI.Layout as Layout
import qualified Game.SystemMap.Component as SystemMap

import App.Common.Rect (Rect(..))
import Core.CoreContext (CoreContext(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.Theme
import Core.UI.UI
import Core.UI.Widgets
import Game.AppState (AppState(..))

import qualified SDL

update :: [SDL.Event] -> AppState -> (ReaderT CoreContext IO (), AppState)
update events appState =
  let
    uiContext = UIContext
      { cursor = Rect 8 (V2 400 200)
      , defaultSize = V2 80 20
      , layoutGap = 4
      , scaleFactor = 1
      , theme = Theme
        { borderColor = V4 191 191 191 255
        , highlightColor = V4 31 171 171 255
        }
      }
    (stateChange, UIState{ renderStack }) = run uiContext events (ui appState)
  in
    ( sequence_ (toList renderStack)
    , stateChange appState
    )

ui :: AppState -> UIComponent AppState
ui appState =
  SystemMap.systemMap appState
  {-
  Layout.vertical
    [ Sized 20 (label gs)
    , Stretched (button "1" (const "1"))
    , Stretched $ Layout.horizontal
      [ DefaultSized (label "A")
      , DefaultSized (label "A")
      , Stretched (button "2" (const "2"))
      , Sized 40 (label "A")
      , Sized 40 (label "A")
      ]
    , DefaultSized (button "3" (const "3"))
    , DefaultSized (button "4" (const "4"))
    ]
    -}
