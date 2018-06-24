module App.GameState where

import App.Prelude

import qualified App.Body as Body
import qualified App.UidMap as UidMap

import App.Body (Body(..))
import App.Camera (Camera(..))
import App.Dims (AU(..), _AU)
import App.Ship (Ship)
import App.Uid (Uid(..))
import App.UidMap (UidMap)

data GameState = GameState
  { bodies :: UidMap Body
  , ships :: UidMap Ship
  , selectedBodyUid :: Maybe (Uid Body)
  , selectedShipUid :: Maybe (Uid Ship)
  , time :: Int
  , movingViewport :: Bool
  , draggedViewport :: Bool
  , camera :: Camera (AU Double) Double
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { bodies = UidMap.fromEntities (view #uid) $
    [ Body.new (Uid 0) "Mercury" 0.38 (2 * pi / (0.24 * 365 * 24 * 60 * 60))
    , Body.new (Uid 1) "Venus" 0.72 (2 * pi / (0.61 * 365 * 24 * 60 * 60))
    , Body.new (Uid 2) "Earth" 1.00 (2 * pi / (365 * 24 * 60 * 60))
    , Body.new (Uid 3) "Mars" 1.52 (2 * pi / (1.88 * 365 * 24 * 60 * 60))
    , Body.new (Uid 4) "Jupiter" 5.20 (2 * pi / (11.86 * 365 * 24 * 60 * 60))
    , Body.new (Uid 5) "Saturn" 9.53 (2 * pi / (29.44 * 365 * 24 * 60 * 60))
    , Body.new (Uid 6) "Uranus" 19.19 (2 * pi / (84.01 * 365 * 24 * 60 * 60))
    , Body.new (Uid 7) "Neptune" 30.06 (2 * pi / (164.79 * 365 * 24 * 60 * 60))
    ]
  , ships = mempty
  , selectedBodyUid = Nothing
  , selectedShipUid = Nothing
  , time = 0
  , movingViewport = False
  , draggedViewport = False
  , camera = Camera 
    { conversion = _AU
    , eyeFrom = V2 0 0
    , eyeTo = V2 640 360
    , scale = V2 200 (-200)
    }
  }
