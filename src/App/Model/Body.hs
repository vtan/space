module App.Model.Body where

import App.Prelude

import qualified Linear as Lin

import App.Dimension.Local (Local(..))
import App.Dimension.Time (Time)
import App.Model.OrbitalState (OrbitalState(..))
import App.Uid (Uid)
import App.UidMap (UidMap)
import Numeric.Extras (fmod)

data Body = Body
  { uid :: Uid Body
  , name :: Text
  , orbitRadius :: Local Double
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

statesAtTime :: Time Int -> Body -> UidMap Body OrbitalState
statesAtTime = relStatesAtTime 0

relStatesAtTime :: V2 (Local Double) -> Time Int -> Body -> UidMap Body OrbitalState
relStatesAtTime origin time Body{ uid, orbitRadius, angularVelocity, children } =
  let phase = fmod (fromIntegral time * angularVelocity) (2 * pi)
      pos = origin + orbitRadius *^ (Local <$> Lin.angle phase)
      state = OrbitalState
        { position = pos
        , orbitCenter = origin
        }
      childStates = children & foldMap (relStatesAtTime pos time)
  in childStates & at uid .~ Just state
