module App.Model.Resource where

import App.Prelude

import qualified App.Model.Installation as Installation

import App.Common.Display (Display, display)
import App.Model.Installation (Installation(..))

data Resource
  = Cadrium
  | Erchanite
  | Tellerite
  | Installation Installation
  deriving (Show, Generic, Eq)

instance Hashable Resource

instance Display Resource where
  display = \case
    Cadrium -> "Cadrium"
    Erchanite -> "Erchanite"
    Tellerite -> "Tellerite"
    Installation _ -> "???"

all :: [Resource]
all =
  minerals
  ++ [Installation i | i <- Installation.all]

minerals :: [Resource]
minerals = [Cadrium, Erchanite, Tellerite]
