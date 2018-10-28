module App.Common.Id where

import App.Prelude

newtype Id i = Id { getInt :: Int }
  deriving (Show, Eq, Ord)
