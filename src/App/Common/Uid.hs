module App.Common.Uid where

import App.Prelude

newtype Uid i = Uid { getInt :: Int }
  deriving (Show, Eq, Ord)
