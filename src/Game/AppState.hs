module Game.AppState
  ( AppState(..) )
where

import GlobalImports

import Game.Dimension.Time (Time)
import App.Model.GameState (GameState)
import Game.UIState (UIState)

data AppState = AppState
  { gameState :: GameState
  , uiState :: UIState
  -- TODO assuming 60 fps
  , timeStep :: Maybe (Time Int)
  }
  deriving (Generic)
