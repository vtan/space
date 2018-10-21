module App.Model.Resource where

import App.Prelude

import qualified App.Model.Installation as Installation

import App.Model.Installation (Installation(..))

data Resource
  = Cadrium
  | Erchanite
  | Tellerite
  | Installation Installation
  deriving (Generic, Eq)

instance Show Resource where
  show = \case
    Cadrium -> "Cadrium"
    Erchanite -> "Erchanite"
    Tellerite -> "Tellerite"
    Installation i -> show i ++ " Installation"

instance Hashable Resource

all :: [Resource]
all =
  minerals
  ++ [Installation i | i <- Installation.all]

minerals :: [Resource]
minerals = [Cadrium, Erchanite, Tellerite]
