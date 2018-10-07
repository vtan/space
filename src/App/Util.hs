module App.Util where

import App.Prelude

import qualified Data.HashMap.Strict as HashMap

import Data.Hashable (Hashable)

clamp :: Ord a => a -> a -> a -> a
clamp mi x ma
  | x < mi = mi
  | x > ma = ma
  | otherwise = x

showDate :: Int -> String
showDate t =
  let secs = t `rem` 60
      mins = t `quot` 60 `rem` 60
      hours = t `quot` 3600 `rem` 24
      day = 1 + t `quot` (24 * 3600) `rem` 30
      month = 1 + t `quot` (30 * 24 * 3600) `rem` 12
      year = 2100 + t `quot` (12 * 30 * 24 * 3600)
  in printf "%d-%02d-%02d %02d:%02d:%02d" year month day hours mins secs

showDuration :: Int -> String
showDuration t
  | t < 60 = printf "%d seconds" t
  | t < 3600 = printf "%.2f minutes" (fromIntegral t / 60 :: Double)
  | t < 24 * 3600 = printf "%.2f hours" (fromIntegral t / 3600 :: Double)
  | otherwise = printf "%.2f days" (fromIntegral t / 24 / 3600 :: Double)

toMap :: (Foldable t, Hashable k, Eq k, Semigroup a) => t (k, a) -> HashMap k a
toMap = flip foldl' HashMap.empty $ \accMap (k, v) ->
  HashMap.insertWith (\new old -> old <> new) k v accMap

reduce :: Foldable t => t a -> (b -> a -> b) -> b -> b
reduce xs f acc0 = foldl' f acc0 xs

whenAlt :: Alternative f => a -> Bool -> f a
whenAlt x b =
  if b then pure x else empty
