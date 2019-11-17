module Core.UI.UI where

import App.Prelude

import App.Common.Rect (Rect(..))
import Core.CoreContext (CoreContext(..))
import Core.UI.Theme (Theme(..))

import qualified SDL

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (State, runState)
import Data.Semigroup (Endo(..))

type UIComponent a = UI (Endo a)

type UI a = ReaderT UIContext (State UIState) a

data UIContext = UIContext
  { cursor :: Rect Double
  , theme :: Theme
  }
  deriving (Generic)

data UIState = UIState
  { events :: [SDL.Event]
  , renderStack :: NonEmpty (ReaderT CoreContext IO ())
  }
  deriving (Generic)

run :: UIContext -> [SDL.Event] -> UIComponent a -> (a -> a, UIState)
run context events component =
  let
    initialState = UIState{ events, renderStack = pure () :| [] }
    (Endo stateChange, uiState) = runState (runReaderT component context) initialState
  in
    (stateChange, uiState)

render :: (CoreContext -> IO ()) -> UI ()
render renderAction =
  modifying #renderStack $ \(currentLayer :| rest) ->
    (currentLayer *> (ReaderT renderAction)) :| rest

absolutePosition :: V2 Double -> UIComponent s -> UIComponent s
absolutePosition pos child =
  local (set (#cursor . #xy) pos) child
