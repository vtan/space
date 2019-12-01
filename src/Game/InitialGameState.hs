module Game.InitialGameState where

import GlobalImports

import qualified Game.Bodies.Body as Body
import qualified Game.Bodies.OrbitTree as OrbitTree
import qualified Game.Bodies.Resource as Resource
import qualified Game.Colonies.Building as Building
import qualified Game.Common.IdMap as IdMap
import qualified Game.Ships.Ship as Ship
import qualified Game.Ships.ShipLogic as ShipLogic

import Game.GameState (GameState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitTree (OrbitTree(..))
import Game.Bodies.ResourceOnBody (ResourceOnBody(..))
import Game.Colonies.Colony (Colony(..))
import Game.Common.Id (Id(..))
import Game.Common.IdMap (IdMap)

initial :: GameState
initial = GameState
  { orbitTree = theOrbitTree
  , bodies = theBodies
  , bodyOrbitalStates = OrbitTree.statesAtTime 0 theOrbitTree
  , colonies = IdMap.fromList
    [ ( Id @Body 2
      , Colony
        { bodyId = Id 2
        , resources = [ (Resource.Resource1, 2500), (Resource.Resource2, 2500) ]
        , buildings =
            [ (Building.Mine Resource.Resource1, 1)
            , (Building.Mine Resource.Resource2, 5)
            , (Building.Factory, 5)
            ]
        , buildOrder = Nothing
        }
      )
    ]
  , ships = mempty
  , time = 0
  }
    & ShipLogic.new (Id 0) (head Ship.designs) (Id 2)

theBodies :: IdMap Body Body
theBodies = IdMap.fromEntities Body.bodyId
  [ Body{ bodyId = Id 8 , name = "Sol", resources = [] }
  , Body{ bodyId = Id 0, name = "Mercury", resources = [] }
  , Body{ bodyId = Id 1, name = "Venus", resources = [] }
  , Body{ bodyId = Id 2, name = "Earth", resources =
      [ (Resource.Resource1, ResourceOnBody{ available = 10000, accessibility = 0.4 })
      , (Resource.Resource2, ResourceOnBody{ available = 5000, accessibility = 0.4 })
      ]
    }
  , Body{ bodyId = Id 9, name = "Luna", resources = [] }
  , Body{ bodyId = Id 3, name = "Mars", resources =
      [ (Resource.Resource1, ResourceOnBody{ available = 10000, accessibility = 0.8 })
      , (Resource.Resource2, ResourceOnBody{ available = 10000, accessibility = 0.8 })
      ]
    }
  , Body{ bodyId = Id 10, name = "Phobos", resources = [] }
  , Body{ bodyId = Id 11, name = "Deimos", resources = [] }
  , Body{ bodyId = Id 4, name = "Jupiter", resources = [] }
  , Body{ bodyId = Id 12, name = "Io", resources = [] }
  , Body{ bodyId = Id 13, name = "Europa", resources = [] }
  , Body{ bodyId = Id 14, name = "Ganymede", resources = [] }
  , Body{ bodyId = Id 15, name = "Callisto", resources = [] }
  , Body{ bodyId = Id 5, name = "Saturn", resources = [] }
  , Body{ bodyId = Id 16, name = "Mimas", resources = [] }
  , Body{ bodyId = Id 17, name = "Enceladus", resources = [] }
  , Body{ bodyId = Id 18, name = "Tethys", resources = [] }
  , Body{ bodyId = Id 19, name = "Dione", resources = [] }
  , Body{ bodyId = Id 20, name = "Rhea", resources = [] }
  , Body{ bodyId = Id 21, name = "Titan", resources = [] }
  , Body{ bodyId = Id 22, name = "Iapetus", resources = [] }
  , Body{ bodyId = Id 6, name = "Uranus", resources = [] }
  , Body{ bodyId = Id 7, name = "Neptune", resources = [] }
  ]

theOrbitTree :: OrbitTree
theOrbitTree = OrbitTree
  { bodyId = Id 8
  , orbitRadius = 0
  , phaseAtEpoch = 0
  , angularVelocity = 0
  , children =
    [ OrbitTree{ bodyId = Id 0, orbitRadius = 0.38, angularVelocity =  2 * pi / (0.24 * 365 * 24 * 60 * 60), phaseAtEpoch = 5.183077334009456, children = [] }
    , OrbitTree{ bodyId = Id 1, orbitRadius = 0.72, angularVelocity =  2 * pi / (0.61 * 365 * 24 * 60 * 60), phaseAtEpoch = 1.2630917966904205, children = [] }
    , OrbitTree{ bodyId = Id 2, orbitRadius = 1.00, angularVelocity =  2 * pi / (365 * 24 * 60 * 60), phaseAtEpoch = 3.150585230297741, children =
        [ OrbitTree{ bodyId = Id 9, orbitRadius = 0.0026, angularVelocity =  2 * pi / (27.32 * 24 * 60 * 60), phaseAtEpoch = 1.1052205264497115, children = [] } ]
      }
    , OrbitTree{ bodyId = Id 3, orbitRadius = 1.52, angularVelocity =  2 * pi / (1.88 * 365 * 24 * 60 * 60), phaseAtEpoch = 4.992778300672891, children =
        [ OrbitTree{ bodyId = Id 10, orbitRadius = 6.2e-5, angularVelocity =  2 * pi / (0.318 * 24 * 60 * 60), phaseAtEpoch = 4.951196230681527, children = [] }
        , OrbitTree{ bodyId = Id 11, orbitRadius = 0.00015, angularVelocity =  2 * pi / (1.263 * 24 * 60 * 60), phaseAtEpoch = 3.1702750214086004, children = [] }
        ]
      }
    , OrbitTree{ bodyId = Id 4, orbitRadius = 5.20, angularVelocity =  2 * pi / (11.86 * 365 * 24 * 60 * 60), phaseAtEpoch = 4.389498795147114, children =
        [ OrbitTree{ bodyId = Id 12, orbitRadius = 0.0028, angularVelocity =  2 * pi / (1.77 * 24 * 60 * 60), phaseAtEpoch = 0.9847674707864948, children = [] }
        , OrbitTree{ bodyId = Id 13, orbitRadius = 0.0044, angularVelocity =  2 * pi / (3.55 * 24 * 60 * 60), phaseAtEpoch = 4.192675332309923, children = [] }
        , OrbitTree{ bodyId = Id 14, orbitRadius = 0.0071, angularVelocity =  2 * pi / (7.15 * 24 * 60 * 60), phaseAtEpoch = 2.7159649277748605, children = [] }
        , OrbitTree{ bodyId = Id 15, orbitRadius = 0.0125, angularVelocity =  2 * pi / (16.69 * 24 * 60 * 60), phaseAtEpoch = 4.856683528845399, children = [] }
        ]
      }
    , OrbitTree{ bodyId = Id 5, orbitRadius = 9.53, angularVelocity =  2 * pi / (29.44 * 365 * 24 * 60 * 60), phaseAtEpoch = 1.3941826461020987, children =
        [ OrbitTree{ bodyId = Id 16, orbitRadius = 0.0012, angularVelocity =  2 * pi / (0.9 * 24 * 60 * 60), phaseAtEpoch = 5.5490471982273215, children = [] }
        , OrbitTree{ bodyId = Id 17, orbitRadius = 0.0016, angularVelocity =  2 * pi / (1.4 * 24 * 60 * 60), phaseAtEpoch = 1.9301490174419995, children = [] }
        , OrbitTree{ bodyId = Id 18, orbitRadius = 0.0020, angularVelocity =  2 * pi / (1.9 * 24 * 60 * 60), phaseAtEpoch = 5.270329258859241, children = [] }
        , OrbitTree{ bodyId = Id 19, orbitRadius = 0.0025, angularVelocity =  2 * pi / (2.7 * 24 * 60 * 60), phaseAtEpoch = 4.654234941931287, children = [] }
        , OrbitTree{ bodyId = Id 20, orbitRadius = 0.0040, angularVelocity =  2 * pi / (4.5 * 24 * 60 * 60), phaseAtEpoch = 4.903102665762759, children = [] }
        , OrbitTree{ bodyId = Id 21, orbitRadius = 0.0081, angularVelocity =  2 * pi / (16 * 24 * 60 * 60), phaseAtEpoch = 1.403922883894238, children = [] }
        , OrbitTree{ bodyId = Id 22, orbitRadius = 0.0238, angularVelocity =  2 * pi / (79 * 24 * 60 * 60), phaseAtEpoch = 5.90320859341022, children = [] }
        ]
      }
    , OrbitTree{ bodyId = Id 6, orbitRadius = 19.19, angularVelocity =  2 * pi / (84.01 * 365 * 24 * 60 * 60), phaseAtEpoch = 5.034906979201371, children = [] }
    , OrbitTree{ bodyId = Id 7, orbitRadius = 30.06, angularVelocity =  2 * pi / (164.79 * 365 * 24 * 60 * 60), phaseAtEpoch = 3.7836608026774474, children = [] }
    ]
  }
