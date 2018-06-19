module App.GameState where

import App.Prelude

import qualified App.UidMap as UidMap

import App.Body (Body(..))
import App.Camera (Camera(..))
import App.Dims (AU(..), _AU)
import App.Uid (Uid(..))
import App.UidMap (UidMap)

data GameState = GameState
  { bodies :: UidMap Body
  , selectedBodyUid :: Maybe (Uid Body)
  , movingViewport :: Bool
  , draggedViewport :: Bool
  , camera :: Camera (AU Double) Double
  , totalTime :: Float
  , quit :: Bool
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { bodies = UidMap.fromEntities (view #uid) $
    [ Body { uid = Uid 0, name = "Mercury", position = 0.38 *^ V2 1 0, orbitRadius = 0.38 }
    , Body { uid = Uid 1, name = "Venus", position = 0.72 *^ V2 0 1, orbitRadius = 0.72 }
    , Body { uid = Uid 2, name = "Earth", position = 1.00 *^ V2 0 1, orbitRadius = 1 }
    , Body { uid = Uid 3, name = "Mars", position = 1.52 *^ V2 0 1, orbitRadius = 1.52 }
    , Body { uid = Uid 4, name = "Jupiter", position = 5.20 *^ V2 0 1, orbitRadius = 5.20 }
    , Body { uid = Uid 5, name = "Saturn", position = 9.53 *^ V2 1 0, orbitRadius = 9.53 }
    , Body { uid = Uid 6, name = "Uranus", position = 19.19 *^ V2 0 1, orbitRadius = 19.19 }
    , Body { uid = Uid 7, name = "Neptune", position = 30.06 *^ V2 0 1, orbitRadius = 30.06 }
    ]
  , selectedBodyUid = Nothing
  , movingViewport = False
  , draggedViewport = False
  , camera = Camera 
    { conversion = _AU
    , eyeFrom = V2 0 0
    , eyeTo = V2 640 360
    , scale = V2 200 (-200)
    }
  , totalTime = 0
  , quit = False
  }
