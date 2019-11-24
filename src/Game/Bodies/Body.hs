module Game.Bodies.Body where

import GlobalImports

import qualified Linear as Lin

import Game.Bodies.OrbitalState (OrbitalState(..))
import Game.Common.Id (Id)
import Game.Common.IdMap (IdMap)
import Game.Dimension.Local (Local(..))
import Game.Dimension.Time (Time)
import Numeric.Extras (fmod)

data Body = Body
  { bodyId :: Id Body
  , name :: Text
  , orbitRadius :: Local Double
  , phaseAtEpoch :: Double
  , angularVelocity :: Double
  , colonyCost :: Maybe Double
  , children :: [Body]
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6

allChildren :: Body -> [Body]
allChildren body@Body{ children } =
  body : concatMap allChildren children

statesAtTime :: Time Int -> Body -> IdMap Body OrbitalState
statesAtTime = relStatesAtTime 0

relStatesAtTime :: V2 (Local Double) -> Time Int -> Body -> IdMap Body OrbitalState
relStatesAtTime origin time Body{ bodyId, orbitRadius, phaseAtEpoch, angularVelocity, children } =
  let phase = (phaseAtEpoch + fromIntegral time * angularVelocity) `fmod` (2 * pi)
      pos = origin + orbitRadius *^ (Local <$> Lin.angle phase)
      state = OrbitalState
        { position = pos
        , orbitCenter = origin
        }
      childStates = children & foldMap (relStatesAtTime pos time)
  in childStates & at bodyId .~ Just state
