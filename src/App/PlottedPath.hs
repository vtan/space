module App.PlottedPath where

import App.Prelude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.Storable as Vector
import qualified Linear as Lin

import App.Dims (AU)
import Data.Vector.Storable (Vector)

data PlottedPath = PlottedPath
  { startTime :: Int 
  , startPos :: V2 (AU Double) 
  , endTime :: Int
  , endPos :: V2 (AU Double)
  , waypoints :: Vector (V2 (AU Double))
  }
  deriving (Generic)

instance Show PlottedPath where
  show PlottedPath{ startTime, endTime } = show (startTime, endTime)

data Waypoint = Waypoint
  { time :: Int
  , position :: V2 (AU Double)
  }
  deriving (Generic)

fromWaypoints :: NonEmpty Waypoint -> PlottedPath
fromWaypoints waypoints =
  let start = NonEmpty.head waypoints
      end = NonEmpty.last waypoints
  in PlottedPath
  { startTime = start ^. #time
  , startPos = start ^. #position
  , endTime = end ^. #time
  , endPos = end ^. #position
  , waypoints = waypoints & fmap (view #position) & toList & Vector.fromList
  }

atTime :: Int -> PlottedPath -> V2 (AU Double)
atTime time PlottedPath{ startTime, startPos, endTime, endPos, waypoints }
  | time <= startTime = startPos
  | time >= endTime = endPos
  | otherwise =
    let i = quot (time - startTime) waypointTime
        timeOfLastWaypoint = startTime + i * waypointTime
        timeSinceLastWaypoint = time - timeOfLastWaypoint
        currentSectionTime
          | (i + 2 == Vector.length waypoints) = endTime - timeOfLastWaypoint
          | otherwise = waypointTime
        ratio = fromIntegral timeSinceLastWaypoint / fromIntegral currentSectionTime
    in case (,) <$> waypoints ^? ix i <*> waypoints ^? ix (i + 1) of
      Just (prevPos, nextPos) -> Lin.lerp ratio nextPos prevPos
      Nothing -> error "Invariant broken in PlottedPath"

waypointTime :: Num a => a
waypointTime = 24 * 3600