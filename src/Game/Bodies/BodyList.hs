module Game.Bodies.BodyList where

import GlobalImports

import qualified Core.UI.Widget as Widget
import qualified Game.Bodies.Body as Body
import qualified Game.Bodies.OrbitTree as OrbitTree

import Core.UI.UI (UIComponent)
import Game.AppState (AppState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitTree (OrbitTree(..))
import Game.Common.Display (display)
import Game.Common.Id (Id)

import Control.Lens (Lens')

bodyList :: Lens' AppState (Maybe (Id Body)) -> Lens' AppState Double -> AppState -> UIComponent AppState
bodyList bodyIdLens bodyIdScrollLens appState@AppState{ gameState } =
  Widget.list
    (Body.bodyId . snd)
    (\(level, Body{ name }) -> fold (replicate level "  │ ") <> display name)
    bodiesWithLevel
    (view bodyIdLens appState)
    (view bodyIdScrollLens appState)
    (set bodyIdLens . Just)
    (set bodyIdScrollLens)
  where
    bodiesWithLevel =
      OrbitTree.depthFirst (view #orbitTree gameState)
        & concatMap \(level, OrbitTree{ bodyId }) ->
          maybeToList (view (#bodies . at bodyId) gameState) & map (level, )
