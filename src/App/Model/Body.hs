module App.Model.Body where

import App.Prelude

import qualified Linear as Lin

import App.Common.Id (Id)
import App.Common.IdMap (IdMap)
import App.Dimension.Local (Local(..))
import App.Dimension.Time (Time)
import App.Model.OrbitalState (OrbitalState(..))
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
