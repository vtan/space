module Game.Common.Id where

import GlobalImports

newtype Id i = Id { getInt :: Int }
  deriving (Show, Eq, Ord)
