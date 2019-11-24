module App.Model.Resource where

import GlobalImports

import qualified App.Model.Installation as Installation

import Game.Common.Display (Display, display)
import App.Model.Installation (Installation(..))

import qualified Data.HashMap.Strict as HashMap

import Linear ((^-^))

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

payResources :: HashMap Resource Double -> HashMap Resource Double -> Maybe (HashMap Resource Double)
payResources resources cost =
  let candidate = HashMap.filter (/= 0) (resources ^-^ cost)
  in if GlobalImports.all (> 0) candidate
  then Just candidate
  else Nothing
