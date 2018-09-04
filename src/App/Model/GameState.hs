module App.Model.GameState where

import App.Prelude

import qualified App.Model.OrbitSystem as OrbitSystem
import qualified App.UidMap as UidMap

import App.Camera (Camera(..))
import App.Model.Body (Body(..))
import App.Model.BodyMinerals (BodyMinerals, MineralData(..))
import App.Model.Colony (Colony(..))
import App.Model.Dims (AU(..), _AU)
import App.Model.OrbitSystem (OrbitSystem(..))
import App.Model.OrbitalState (OrbitalState)
import App.Model.Ship (Ship)
import App.Uid (Uid(..))
import App.UidMap (UidMap)

data GameState = GameState
  { bodies :: UidMap Body Body
  , orbitSystem :: OrbitSystem
  , bodyOrbitalStates :: UidMap Body OrbitalState
  , bodyMinerals :: UidMap Body BodyMinerals
  , colonies :: UidMap Body Colony
  , ships :: UidMap Ship Ship
  , time :: Int
  , timeStepPerFrame :: Maybe Int
  , movingViewport :: Bool
  , camera :: Camera (AU Double) Double
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { bodies = theBodies
  , orbitSystem = theOrbitSystem
  , bodyOrbitalStates = OrbitSystem.statesAtTime 0 theOrbitSystem
  , bodyMinerals = UidMap.fromList
    [ (Uid @Body 2, [(0, MineralData{ available = 10, accessibility = 0.6 })])
    , (Uid @Body 3, [(0, MineralData{ available = 5, accessibility = 0.85 })])
    ]
  , colonies = UidMap.fromList 
    [(Uid @Body 2, Colony{ stockpile = [(0, 2.5)], mines = [(0, 1)], buildingTask = Nothing })]
  , ships = mempty
  , time = 0
  , timeStepPerFrame = Nothing
  , movingViewport = False
  , camera = Camera 
    { conversion = _AU
    , eyeFrom = V2 0 0
    , eyeTo = V2 640 360
    , scale = V2 200 (-200)
    }
  }

theBodies :: UidMap Body Body
theBodies = UidMap.fromEntities (view #uid)
  [ Body (Uid 0) "Mercury"
  , Body (Uid 1) "Venus"
  , Body (Uid 2) "Earth"
  , Body (Uid 3) "Mars"
  , Body (Uid 4) "Jupiter"
  , Body (Uid 5) "Saturn"
  , Body (Uid 6) "Uranus"
  , Body (Uid 7) "Neptune"
  , Body (Uid 8) "Sol"
  , Body (Uid 9) "Luna"
  , Body (Uid 10) "Phobos"
  , Body (Uid 11) "Deimos"
  ]

theOrbitSystem :: OrbitSystem
theOrbitSystem = OrbitSystem
  { body = Uid 8
  , orbitRadius = 0
  , angularVelocity = 0
  , children = 
    [ OrbitSystem{ body = Uid 0, orbitRadius = 0.38, angularVelocity =  2 * pi / (0.24 * 365 * 24 * 60 * 60), children = [] }
    , OrbitSystem{ body = Uid 1, orbitRadius = 0.72, angularVelocity =  2 * pi / (0.61 * 365 * 24 * 60 * 60), children = [] }
    , OrbitSystem{ body = Uid 2, orbitRadius = 1.00, angularVelocity =  2 * pi / (365 * 24 * 60 * 60), children = 
        [ OrbitSystem{ body = Uid 9, orbitRadius = 0.0026, angularVelocity =  2 * pi / (27.32 * 24 * 60 * 60), children = [] } ]
      }
    , OrbitSystem{ body = Uid 3, orbitRadius = 1.52, angularVelocity =  2 * pi / (1.88 * 365 * 24 * 60 * 60), children = 
        [ OrbitSystem{ body = Uid 10, orbitRadius = 6.2e-5, angularVelocity =  2 * pi / (0.318 * 24 * 60 * 60), children = [] }
        , OrbitSystem{ body = Uid 11, orbitRadius = 0.00015, angularVelocity =  2 * pi / (1.263 * 24 * 60 * 60), children = [] }
        ]
      }
    , OrbitSystem{ body = Uid 4, orbitRadius = 5.20, angularVelocity =  2 * pi / (11.86 * 365 * 24 * 60 * 60), children = [] }
    , OrbitSystem{ body = Uid 5, orbitRadius = 9.53, angularVelocity =  2 * pi / (29.44 * 365 * 24 * 60 * 60), children = [] }
    , OrbitSystem{ body = Uid 6, orbitRadius = 19.19, angularVelocity =  2 * pi / (84.01 * 365 * 24 * 60 * 60), children = [] }
    , OrbitSystem{ body = Uid 7, orbitRadius = 30.06, angularVelocity =  2 * pi / (164.79 * 365 * 24 * 60 * 60), children = [] }
    ]
  }