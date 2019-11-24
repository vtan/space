module Game.AppState
  ( AppState(..) )
where

import GlobalImports

import Game.GameState (GameState)
import Game.UIState (UIState)
import Game.Dimension.Time (Time)

data AppState = AppState
  { gameState :: GameState
  , uiState :: UIState
  -- TODO assuming 60 fps
  , timeStep :: Maybe (Time Int)
  }
  deriving (Generic)
