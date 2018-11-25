module App.Update.Initial where

import App.Prelude

import qualified App.Common.IdMap as IdMap
import qualified App.Logic.ShipBuilding as Logic.ShipBuilding
import qualified App.Model.Body as Body
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified Data.HashMap.Strict as HashMap

import App.Common.Id (Id(..))
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))

gameState :: GameState
gameState = GameState
  { rootBody = theRootBody
  , bodies = Body.allChildren theRootBody
      & map (\body@Body{ bodyId } -> (bodyId, body))
      & IdMap.fromList
  , bodyOrbitalStates = Body.statesAtTime 0 theRootBody
  , bodyMinerals = IdMap.fromList
    [ ( Id @Body 2
      , [ (Resource.Cadrium, Mineral{ available = 10000, accessibility = 0.4 })
        , (Resource.Erchanite, Mineral{ available = 5000, accessibility = 0.4 })
        , (Resource.Tellerite, Mineral{ available = 2000, accessibility = 0.4 })
        ]
      )
    , ( Id @Body 3
      , [ (Resource.Cadrium, Mineral{ available = 10000, accessibility = 0.8 })
        , (Resource.Erchanite, Mineral{ available = 10000, accessibility = 0.8 })
        , (Resource.Tellerite, Mineral{ available = 10000, accessibility = 0.8 })
        ]
      )
    ]
  , colonies = IdMap.fromList
    [ ( Id @Body 2
      , Colony
        { bodyId = Id 2
        , population = 10000000000
        , isHomeworld = True
        , stockpile = [ (Resource.Cadrium, 2500), (Resource.Erchanite, 2500), (Resource.Tellerite, 2500) ]
        , installations = [ (Installation.Mine, 1), (Installation.Factory, 5) ]
        , buildQueue = []
        , shipBuildingTask = Nothing
        , miningPriorities = zip Resource.minerals (repeat 1) & HashMap.fromList
        }
      )
    ]
  , ships = IdMap.fromEntities (view #shipId)
      ( Body.statesAtTime 0 theRootBody ^.. at (Id 2) . _Just <&> \orbitalState ->
          Logic.ShipBuilding.shipBuiltAt (Id 2) orbitalState (Id 0) Ship.FreighterType 1
      )
  , time = 0
  }

theRootBody :: Body
theRootBody = Body
  { bodyId = Id 8
  , name = "Sol"
  , colonyCost = Nothing
  , orbitRadius = 0
  , phaseAtEpoch = 0
  , angularVelocity = 0
  , children =
    [ Body{ bodyId = Id 0, name = "Mercury", colonyCost = Just 8,  orbitRadius = 0.38, angularVelocity =  2 * pi / (0.24 * 365 * 24 * 60 * 60), phaseAtEpoch = 5.183077334009456, children = [] }
    , Body{ bodyId = Id 1, name = "Venus", colonyCost = Just 16,  orbitRadius = 0.72, angularVelocity =  2 * pi / (0.61 * 365 * 24 * 60 * 60), phaseAtEpoch = 1.2630917966904205, children = [] }
    , Body{ bodyId = Id 2, name = "Earth", colonyCost = Just 1,  orbitRadius = 1.00, angularVelocity =  2 * pi / (365 * 24 * 60 * 60), phaseAtEpoch = 3.150585230297741, children =
        [ Body{ bodyId = Id 9, name = "Luna", colonyCost = Just 2.5,  orbitRadius = 0.0026, angularVelocity =  2 * pi / (27.32 * 24 * 60 * 60), phaseAtEpoch = 1.1052205264497115, children = [] } ]
      }
    , Body{ bodyId = Id 3, name = "Mars", colonyCost = Just 2,  orbitRadius = 1.52, angularVelocity =  2 * pi / (1.88 * 365 * 24 * 60 * 60), phaseAtEpoch = 4.992778300672891, children =
        [ Body{ bodyId = Id 10, name = "Phobos", colonyCost = Just 2.5,  orbitRadius = 6.2e-5, angularVelocity =  2 * pi / (0.318 * 24 * 60 * 60), phaseAtEpoch = 4.951196230681527, children = [] }
        , Body{ bodyId = Id 11, name = "Deimos", colonyCost = Just 2.5,  orbitRadius = 0.00015, angularVelocity =  2 * pi / (1.263 * 24 * 60 * 60), phaseAtEpoch = 3.1702750214086004, children = [] }
        ]
      }
    , Body{ bodyId = Id 4, name = "Jupiter", colonyCost = Nothing,  orbitRadius = 5.20, angularVelocity =  2 * pi / (11.86 * 365 * 24 * 60 * 60), phaseAtEpoch = 4.389498795147114, children =
        [ Body{ bodyId = Id 12, name = "Io", colonyCost = Just 4,  orbitRadius = 0.0028, angularVelocity =  2 * pi / (1.77 * 24 * 60 * 60), phaseAtEpoch = 0.9847674707864948, children = [] }
        , Body{ bodyId = Id 13, name = "Europa", colonyCost = Just 4,  orbitRadius = 0.0044, angularVelocity =  2 * pi / (3.55 * 24 * 60 * 60), phaseAtEpoch = 4.192675332309923, children = [] }
        , Body{ bodyId = Id 14, name = "Ganymede", colonyCost = Just 4,  orbitRadius = 0.0071, angularVelocity =  2 * pi / (7.15 * 24 * 60 * 60), phaseAtEpoch = 2.7159649277748605, children = [] }
        , Body{ bodyId = Id 15, name = "Callisto", colonyCost = Just 4,  orbitRadius = 0.0125, angularVelocity =  2 * pi / (16.69 * 24 * 60 * 60), phaseAtEpoch = 4.856683528845399, children = [] }
        ]
      }
    , Body{ bodyId = Id 5, name = "Saturn", colonyCost = Nothing,  orbitRadius = 9.53, angularVelocity =  2 * pi / (29.44 * 365 * 24 * 60 * 60), phaseAtEpoch = 1.3941826461020987, children =
        [ Body{ bodyId = Id 16, name = "Mimas", colonyCost = Just 5,  orbitRadius = 0.0012, angularVelocity =  2 * pi / (0.9 * 24 * 60 * 60), phaseAtEpoch = 5.5490471982273215, children = [] }
        , Body{ bodyId = Id 17, name = "Enceladus", colonyCost = Just 5,  orbitRadius = 0.0016, angularVelocity =  2 * pi / (1.4 * 24 * 60 * 60), phaseAtEpoch = 1.9301490174419995, children = [] }
        , Body{ bodyId = Id 18, name = "Tethys", colonyCost = Just 5,  orbitRadius = 0.0020, angularVelocity =  2 * pi / (1.9 * 24 * 60 * 60), phaseAtEpoch = 5.270329258859241, children = [] }
        , Body{ bodyId = Id 19, name = "Dione", colonyCost = Just 5,  orbitRadius = 0.0025, angularVelocity =  2 * pi / (2.7 * 24 * 60 * 60), phaseAtEpoch = 4.654234941931287, children = [] }
        , Body{ bodyId = Id 20, name = "Rhea", colonyCost = Just 5,  orbitRadius = 0.0040, angularVelocity =  2 * pi / (4.5 * 24 * 60 * 60), phaseAtEpoch = 4.903102665762759, children = [] }
        , Body{ bodyId = Id 21, name = "Titan", colonyCost = Just 3,  orbitRadius = 0.0081, angularVelocity =  2 * pi / (16 * 24 * 60 * 60), phaseAtEpoch = 1.403922883894238, children = [] }
        , Body{ bodyId = Id 22, name = "Iapetus", colonyCost = Just 5,  orbitRadius = 0.0238, angularVelocity =  2 * pi / (79 * 24 * 60 * 60), phaseAtEpoch = 5.90320859341022, children = [] }
        ]
      }
    , Body{ bodyId = Id 6, name = "Uranus", colonyCost = Nothing,  orbitRadius = 19.19, angularVelocity =  2 * pi / (84.01 * 365 * 24 * 60 * 60), phaseAtEpoch = 5.034906979201371, children = [] }
    , Body{ bodyId = Id 7, name = "Neptune", colonyCost = Nothing,  orbitRadius = 30.06, angularVelocity =  2 * pi / (164.79 * 365 * 24 * 60 * 60), phaseAtEpoch = 3.7836608026774474, children = [] }
    ]
  }
