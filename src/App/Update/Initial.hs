module App.Update.Initial where

import App.Prelude

import qualified App.Model.Body as Body
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Update.Logic as Logic
import qualified App.UidMap as UidMap

import App.Camera (Camera(..))
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.Dims (_AU)
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))
import App.Uid (Uid(..))

gameState :: GameState
gameState = GameState
  { rootBody = theRootBody
  , bodies = Body.allChildren theRootBody
      & map (\body@Body{ uid } -> (uid, body))
      & UidMap.fromList
  , bodyOrbitalStates = Body.statesAtTime 0 theRootBody
  , bodyMinerals = UidMap.fromList
    [ (Uid @Body 2, [(Resource.Mineral, Mineral{ available = 10000, accessibility = 0.6 })])
    , (Uid @Body 3, [(Resource.Mineral, Mineral{ available = 5000, accessibility = 0.8 })])
    ]
  , colonies = UidMap.fromList
    [ ( Uid @Body 2
      , Colony
        { stockpile = [(Resource.Mineral, 2500)]
        , installations = [(Installation.Mine, 500)]
        , buildingTask = Nothing
        , shipBuildingTask = Nothing
        }
      )
    ]
  , ships = UidMap.fromEntities (view #uid)
      ( Body.statesAtTime 0 theRootBody ^.. at (Uid 2) . _Just <&> \orbitalState ->
          Logic.shipBuiltAt (Uid 2) orbitalState (Uid 0)
      )
  , time = 0
  , timeStepPerFrame = Nothing
  , movingViewport = False
  , camera = Camera
    { conversion = _AU
    , eyeFrom = V2 0 0
    , eyeTo = 0.5 *^ V2 1728 972
    , scale = V2 200 (-200)
    }
  }

theRootBody :: Body
theRootBody = Body
  { uid = Uid 8
  , name = "Sol"
  , orbitRadius = 0
  , angularVelocity = 0
  , children =
    [ Body{ uid = Uid 0, name = "Mercury", orbitRadius = 0.38, angularVelocity =  2 * pi / (0.24 * 365 * 24 * 60 * 60), children = [] }
    , Body{ uid = Uid 1, name = "Venus", orbitRadius = 0.72, angularVelocity =  2 * pi / (0.61 * 365 * 24 * 60 * 60), children = [] }
    , Body{ uid = Uid 2, name = "Earth", orbitRadius = 1.00, angularVelocity =  2 * pi / (365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 9, name = "Luna", orbitRadius = 0.0026, angularVelocity =  2 * pi / (27.32 * 24 * 60 * 60), children = [] } ]
      }
    , Body{ uid = Uid 3, name = "Mars", orbitRadius = 1.52, angularVelocity =  2 * pi / (1.88 * 365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 10, name = "Phobos", orbitRadius = 6.2e-5, angularVelocity =  2 * pi / (0.318 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 11, name = "Deimos", orbitRadius = 0.00015, angularVelocity =  2 * pi / (1.263 * 24 * 60 * 60), children = [] }
        ]
      }
    , Body{ uid = Uid 4, name = "Jupiter", orbitRadius = 5.20, angularVelocity =  2 * pi / (11.86 * 365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 12, name = "Io", orbitRadius = 0.0028, angularVelocity =  2 * pi / (1.77 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 13, name = "Europa", orbitRadius = 0.0044, angularVelocity =  2 * pi / (3.55 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 14, name = "Ganymede", orbitRadius = 0.0071, angularVelocity =  2 * pi / (7.15 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 15, name = "Callisto", orbitRadius = 0.0125, angularVelocity =  2 * pi / (16.69 * 24 * 60 * 60), children = [] }
        ]
      }
    , Body{ uid = Uid 5, name = "Saturn", orbitRadius = 9.53, angularVelocity =  2 * pi / (29.44 * 365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 16, name = "Mimas", orbitRadius = 0.0012, angularVelocity =  2 * pi / (0.9 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 17, name = "Enceladus", orbitRadius = 0.0016, angularVelocity =  2 * pi / (1.4 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 18, name = "Tethys", orbitRadius = 0.0020, angularVelocity =  2 * pi / (1.9 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 19, name = "Dione", orbitRadius = 0.0025, angularVelocity =  2 * pi / (2.7 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 20, name = "Rhea", orbitRadius = 0.0040, angularVelocity =  2 * pi / (4.5 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 21, name = "Titan", orbitRadius = 0.0081, angularVelocity =  2 * pi / (16 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 22, name = "Iapetus", orbitRadius = 0.0238, angularVelocity =  2 * pi / (79 * 24 * 60 * 60), children = [] }
        ]
      }
    , Body{ uid = Uid 6, name = "Uranus", orbitRadius = 19.19, angularVelocity =  2 * pi / (84.01 * 365 * 24 * 60 * 60), children = [] }
    , Body{ uid = Uid 7, name = "Neptune", orbitRadius = 30.06, angularVelocity =  2 * pi / (164.79 * 365 * 24 * 60 * 60), children = [] }
    ]
  }
