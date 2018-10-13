module App.Common.Util where

import App.Prelude

import qualified Data.HashMap.Strict as HashMap

import Data.Hashable (Hashable)

clamp :: Ord a => a -> a -> a -> a
clamp mi x ma
  | x < mi = mi
  | x > ma = ma
  | otherwise = x

toMap :: (Foldable t, Hashable k, Eq k, Semigroup a) => t (k, a) -> HashMap k a
toMap = flip foldl' HashMap.empty $ \accMap (k, v) ->
  HashMap.insertWith (\new old -> old <> new) k v accMap

reduce :: Foldable t => t a -> (b -> a -> b) -> b -> b
reduce xs f acc0 = foldl' f acc0 xs

whenAlt :: Alternative f => a -> Bool -> f a
whenAlt x b =
  if b then pure x else empty
