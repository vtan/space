module Game.AppState where

import App.Prelude

import App.Model.GameState (GameState)
import Game.UIState (UIState)

data AppState = AppState
  { gameState :: GameState
  , uiState :: UIState
  }
  deriving (Generic)