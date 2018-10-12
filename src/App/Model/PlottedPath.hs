module App.Model.PlottedPath
  ( PlottedPath(..)
  , plot, atTime
  )
where

import App.Prelude

import qualified App.Dimension.Speed as Speed
import qualified App.Dimension.Time as Time
import qualified App.Model.Body as Body
import qualified Linear as Lin

import App.Dimension.Local (Local)
import App.Dimension.Speed (Speed)
import App.Dimension.Time (Time)
import App.Model.Body (Body)
import App.Uid (Uid)

data PlottedPath = PlottedPath
  { startTime :: Time Int
  , startPos :: V2 (Local Double)
  , endTime :: Time Int
  , endPos :: V2 (Local Double)
  }
  deriving (Generic, Show)

plot :: Time Int -> V2 (Local Double) -> Speed Double -> Uid Body -> Body -> Maybe PlottedPath
plot startTime startPos speed bodyUid rootBody = do
  (beforeCrudeApprox, _) <- approxArrival (16 & Time.hours) 0
  (beforeFinerApprox, _) <- approxArrival (30 & Time.minutes) beforeCrudeApprox
  (_, endDtime) <- approxArrival Time.oneSecond beforeFinerApprox
  let
    endTime = startTime + endDtime
    endPos = Body.statesAtTime endTime rootBody ^?! at bodyUid . _Just . #position
  pure PlottedPath{ startTime, startPos, endTime, endPos }
  where
    approxArrival :: Time Int -> Time Int -> Maybe (Time Int, Time Int)
    approxArrival !timeStep !dtime =
      let !reachSq =
            let reach = dtime `Speed.mul` speed
            in reach * reach
          !distSq =
            let time = startTime + dtime
                pos = Body.statesAtTime time rootBody ^?! at bodyUid . _Just . #position
            in Lin.qd startPos pos
          !dtime' = dtime + timeStep
          approxTime =
            let lastReach = (dtime - timeStep) `Speed.mul` speed
                lastReachSq = lastReach * lastReach
                ratio = sqrt $ (distSq - lastReachSq) / (reachSq - lastReachSq)
            in dtime - timeStep + round (ratio * fromIntegral timeStep)
      in if
        | reachSq >= distSq -> Just (dtime - timeStep, approxTime)
        | dtime' <= maxSearchTime -> approxArrival timeStep dtime'
        | otherwise -> Nothing

atTime :: Time Int -> PlottedPath -> V2 (Local Double)
atTime time PlottedPath{ startTime, startPos, endTime, endPos }
  | time <= startTime = startPos
  | time >= endTime = endPos
  | otherwise =
    let ratio = fromIntegral (time - startTime) / fromIntegral (endTime - startTime)
    in Lin.lerp ratio endPos startPos

maxSearchTime :: Num a => a
maxSearchTime = 2048 * 24 * 3600
