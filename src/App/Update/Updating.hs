module App.Update.Updating where

import App.Prelude

import qualified App.UIBuilder.UIBuilder as UIBuilder
import qualified App.Update.UIState as UIState
import qualified SDL

import App.Dimension.Time (Time)
import App.UIBuilder.UIBuilder (UIBuilderContext, UIBuilderState)
import App.Update.Events
import App.Update.UIState (UIState)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)

type Updating a = ReaderT Context (StateT State Identity) a

data Context = Context
  { uiBuilderContext :: UIBuilderContext }
  deriving (Show, Generic)

data State = State
  { timeStepPerFrame :: Maybe (Time Int)
  , movingViewport :: Bool
  , quit :: Bool
  , newScaleFactor :: Maybe Int
  , reloadResources :: Bool
  , uiBuilderState :: UIBuilderState
  , ui :: UIState
  }
  deriving (Generic)

initialState :: State
initialState = State
  { timeStepPerFrame = Nothing
  , movingViewport = False
  , quit = False
  , newScaleFactor = Nothing
  , reloadResources = False
  , uiBuilderState = UIBuilder.initialState
  , ui = UIState.initial
  }

runFrame :: [SDL.Event] -> Context -> State -> Updating a -> (a, State)
runFrame events ctx st u =
  -- TODO clean this up
  let st' = st
        & #newScaleFactor .~ Nothing
        & #quit .~ any (\case QuitEvent -> True; _ -> False) events
        & #uiBuilderState . #events .~ events
        & #uiBuilderState . #renderStack .~ pure () :| []
        & #uiBuilderState . #groups .~ UIBuilder.rootGroup :| []
      Identity (a, st'') = runStateT (runReaderT u ctx) st'
  in (a, st'')
