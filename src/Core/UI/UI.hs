module Core.UI.UI where

import GlobalImports

import qualified Core.Common.Rect as Rect

import Core.Common.Rect (Rect(..))
import Core.CoreContext (CoreContext(..))
import Core.UI.Theme (Theme(..))

import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified SDL

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State, runState)
import Data.Semigroup (Endo(..))

type UIComponent a = UI (Endo a)

type UI a = ReaderT UIContext (State UIState) a

data UIContext = UIContext
  { cursor :: Rect Double
  , defaultSize :: V2 Double
  , layoutGap :: Double
  , scaleFactor :: Double
  , scaledScreenSize :: V2 Double
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

empty :: UIComponent s
empty =
  pure mempty

concat :: [UIComponent s] -> UIComponent s
concat =
  fmap fold . sequence

consumeEvents :: (SDL.Event -> Bool) -> UI [SDL.Event]
consumeEvents predicate = do
  allEvents <- State.gets events
  let (consumed, remaining) = List.partition predicate allEvents
  State.modify' (set #events remaining)
  pure consumed

render :: ReaderT CoreContext IO () -> UI ()
render renderAction =
  modifying #renderStack $ \(currentLayer :| rest) ->
    (currentLayer *> renderAction) :| rest

pushRenderStack :: UI ()
pushRenderStack =
  modifying #renderStack \(currentLayer :| rest) ->
    pure () :| (currentLayer : rest)

scaleRect :: Rect Double -> UI (SDL.Rectangle CInt)
scaleRect rect =
  ask & fmap \UIContext{ scaleFactor } ->
    Rect.toSdl (fmap (round @Double @CInt) (scaleFactor *^ rect))

cursorAt :: Rect Double -> UIComponent s -> UIComponent s
cursorAt localCursor child =
  local (set #cursor localCursor) child
