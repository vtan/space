module App.Common.Util where

import App.Prelude

clamp :: Ord a => a -> a -> a -> a
clamp mi x ma
  | x < mi = mi
  | x > ma = ma
  | otherwise = x

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe x b =
  if b then Just x else Nothing
