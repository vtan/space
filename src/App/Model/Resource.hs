module App.Model.Resource where

import App.Prelude

import qualified App.Model.Installation as Installation
import qualified Data.List as List

import App.Model.Installation (Installation(..))
import Data.String (fromString)

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

shortName :: Resource -> String
shortName = \case
    Cadrium -> "Cad"
    Erchanite -> "Ern"
    Tellerite -> "Tlr"
    Installation _ -> "Inst"

printCost :: HashMap Resource Double -> Text
printCost =
  itoList
  >>> map (\(resource, mass) -> printf "%.0f %s" mass (shortName resource))
  >>> List.intercalate ", "
  >>> fromString
