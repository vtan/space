module App.PlottedPath where

import App.Prelude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.Storable as Vector

import App.Dims (AU)
import Data.Vector.Storable (Vector)

data PlottedPath = PlottedPath
  { start :: Int
  , end :: Int
  , waypoints :: Vector (V2 (AU Double))
  }
  deriving (Generic)

instance Show PlottedPath where
  show PlottedPath{ start, end } = show (start, end)

data Waypoint = Waypoint
  { time :: Int
  , position :: V2 (AU Double)
  }
  deriving (Generic)

fromWaypoints :: NonEmpty Waypoint -> PlottedPath
fromWaypoints waypoints = PlottedPath
  { start = NonEmpty.head waypoints ^. #time
  , end = NonEmpty.last waypoints ^. #time
  , waypoints = waypoints & fmap (view #position) & toList & Vector.fromList
  }

waypointTime :: Num a => a
waypointTime = 24 * 3600