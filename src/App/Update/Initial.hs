module App.Update.Initial where

import App.Prelude

import qualified App.Common.UidMap as UidMap
import qualified App.Logic.Colony as Logic.Colony
import qualified App.Model.Body as Body
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource

import App.Common.Uid (Uid(..))
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))

gameState :: GameState
gameState = GameState
  { rootBody = theRootBody
  , bodies = Body.allChildren theRootBody
      & map (\body@Body{ uid } -> (uid, body))
      & UidMap.fromList
  , bodyOrbitalStates = Body.statesAtTime 0 theRootBody
  , bodyMinerals = UidMap.fromList
    [ ( Uid @Body 2
      , [ (Resource.Cadrium, Mineral{ available = 10000, accessibility = 0.4 })
        , (Resource.Erchanite, Mineral{ available = 5000, accessibility = 0.4 })
        , (Resource.Tellerite, Mineral{ available = 2000, accessibility = 0.4 })
        ]
      )
    , ( Uid @Body 3
      , [ (Resource.Cadrium, Mineral{ available = 10000, accessibility = 0.8 })
        , (Resource.Erchanite, Mineral{ available = 10000, accessibility = 0.8 })
        , (Resource.Tellerite, Mineral{ available = 10000, accessibility = 0.8 })
        ]
      )
    ]
  , colonies = UidMap.fromList
    [ ( Uid @Body 2
      , Colony
        { population = 10000000000
        , isHomeworld = True
        , stockpile = [ (Resource.Cadrium, 2500), (Resource.Erchanite, 2500), (Resource.Tellerite, 2500) ]
        , installations = [(Installation.Mine, 500)]
        , buildingTask = Nothing
        , shipBuildingTask = Nothing
        }
      )
    ]
  , ships = UidMap.fromEntities (view #uid)
      ( Body.statesAtTime 0 theRootBody ^.. at (Uid 2) . _Just <&> \orbitalState ->
          Logic.Colony.shipBuiltAt (Uid 2) orbitalState (Uid 0)
      )
  , time = 0
  }

theRootBody :: Body
theRootBody = Body
  { uid = Uid 8
  , name = "Sol"
  , colonyCost = Nothing
  , orbitRadius = 0
  , angularVelocity = 0
  , children =
    [ Body{ uid = Uid 0, name = "Mercury", colonyCost = Just 8,  orbitRadius = 0.38, angularVelocity =  2 * pi / (0.24 * 365 * 24 * 60 * 60), children = [] }
    , Body{ uid = Uid 1, name = "Venus", colonyCost = Just 16,  orbitRadius = 0.72, angularVelocity =  2 * pi / (0.61 * 365 * 24 * 60 * 60), children = [] }
    , Body{ uid = Uid 2, name = "Earth", colonyCost = Just 1,  orbitRadius = 1.00, angularVelocity =  2 * pi / (365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 9, name = "Luna", colonyCost = Just 2.5,  orbitRadius = 0.0026, angularVelocity =  2 * pi / (27.32 * 24 * 60 * 60), children = [] } ]
      }
    , Body{ uid = Uid 3, name = "Mars", colonyCost = Just 2,  orbitRadius = 1.52, angularVelocity =  2 * pi / (1.88 * 365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 10, name = "Phobos", colonyCost = Just 2.5,  orbitRadius = 6.2e-5, angularVelocity =  2 * pi / (0.318 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 11, name = "Deimos", colonyCost = Just 2.5,  orbitRadius = 0.00015, angularVelocity =  2 * pi / (1.263 * 24 * 60 * 60), children = [] }
        ]
      }
    , Body{ uid = Uid 4, name = "Jupiter", colonyCost = Nothing,  orbitRadius = 5.20, angularVelocity =  2 * pi / (11.86 * 365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 12, name = "Io", colonyCost = Just 4,  orbitRadius = 0.0028, angularVelocity =  2 * pi / (1.77 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 13, name = "Europa", colonyCost = Just 4,  orbitRadius = 0.0044, angularVelocity =  2 * pi / (3.55 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 14, name = "Ganymede", colonyCost = Just 4,  orbitRadius = 0.0071, angularVelocity =  2 * pi / (7.15 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 15, name = "Callisto", colonyCost = Just 4,  orbitRadius = 0.0125, angularVelocity =  2 * pi / (16.69 * 24 * 60 * 60), children = [] }
        ]
      }
    , Body{ uid = Uid 5, name = "Saturn", colonyCost = Nothing,  orbitRadius = 9.53, angularVelocity =  2 * pi / (29.44 * 365 * 24 * 60 * 60), children =
        [ Body{ uid = Uid 16, name = "Mimas", colonyCost = Just 5,  orbitRadius = 0.0012, angularVelocity =  2 * pi / (0.9 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 17, name = "Enceladus", colonyCost = Just 5,  orbitRadius = 0.0016, angularVelocity =  2 * pi / (1.4 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 18, name = "Tethys", colonyCost = Just 5,  orbitRadius = 0.0020, angularVelocity =  2 * pi / (1.9 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 19, name = "Dione", colonyCost = Just 5,  orbitRadius = 0.0025, angularVelocity =  2 * pi / (2.7 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 20, name = "Rhea", colonyCost = Just 5,  orbitRadius = 0.0040, angularVelocity =  2 * pi / (4.5 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 21, name = "Titan", colonyCost = Just 3,  orbitRadius = 0.0081, angularVelocity =  2 * pi / (16 * 24 * 60 * 60), children = [] }
        , Body{ uid = Uid 22, name = "Iapetus", colonyCost = Just 5,  orbitRadius = 0.0238, angularVelocity =  2 * pi / (79 * 24 * 60 * 60), children = [] }
        ]
      }
    , Body{ uid = Uid 6, name = "Uranus", colonyCost = Nothing,  orbitRadius = 19.19, angularVelocity =  2 * pi / (84.01 * 365 * 24 * 60 * 60), children = [] }
    , Body{ uid = Uid 7, name = "Neptune", colonyCost = Nothing,  orbitRadius = 30.06, angularVelocity =  2 * pi / (164.79 * 365 * 24 * 60 * 60), children = [] }
    ]
  }
