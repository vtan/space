module App.Model.Resource where

import App.Prelude

import App.Model.Installation (Installation(..))

data Resource
  = Mineral
  | Installation Installation
  deriving (Generic, Eq)

instance Show Resource where
  show = \case
    Mineral -> "Mineral"
    Installation i -> show i ++ " Installation"

instance Hashable Resource

all :: [Resource]
all =
  [Mineral]
  ++ [Installation i | i <- [minBound .. maxBound]]
