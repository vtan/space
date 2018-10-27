module App.Common.Util where

import App.Prelude

clamp :: Ord a => a -> a -> a -> a
clamp mi x ma
  | x < mi = mi
  | x > ma = ma
  | otherwise = x

whenAlt :: Alternative f => a -> Bool -> f a
whenAlt x b =
  if b then pure x else empty
