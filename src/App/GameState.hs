module App.GameState where

import App.Prelude

import App.Body (Body(..))
import App.Camera (Camera(..))
import App.Dims (AU(..), _AU)

data GameState = GameState
  { bodies :: [Body]
  , movingViewport :: Bool
  , camera :: Camera (AU Double) Double
  , totalTime :: Float
  , quit :: Bool
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { bodies =
    [ Body { position = 0.38 *^ V2 1 0, orbitRadius = 0.38 }
    , Body { position = 0.72 *^ V2 0 1, orbitRadius = 0.72 }
    , Body { position = 1.00 *^ V2 0 1, orbitRadius = 1 }
    , Body { position = 1.52 *^ V2 0 1, orbitRadius = 1.52 }
    , Body { position = 5.20 *^ V2 0 1, orbitRadius = 5.20 }
    , Body { position = 9.53 *^ V2 1 0, orbitRadius = 9.53 }
    , Body { position = 19.19 *^ V2 0 1, orbitRadius = 19.19 }
    , Body { position = 30.96 *^ V2 0 1, orbitRadius = 30.06 }
    ]
  , movingViewport = False
  , camera = Camera 
    { conversion = _AU
    , scale = 48
    , translate = 16
    }
  , totalTime = 0
  , quit = False
  }
