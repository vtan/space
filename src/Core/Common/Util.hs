module Core.Common.Util where

import GlobalImports

import Control.Lens (Lens')

clamp :: Ord a => a -> a -> a -> a
clamp mi x ma
  | x < mi = mi
  | x > ma = ma
  | otherwise = x

_nonEmptyHead :: Lens' (NonEmpty a) a
_nonEmptyHead f (x :| xs) =
  f x <&> (:| xs)
