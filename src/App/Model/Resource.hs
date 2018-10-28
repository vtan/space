module App.Model.Resource where

import App.Prelude

import qualified App.Common.Print as Print
import qualified App.Model.Installation as Installation
import qualified Data.List as List

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

shortName :: Resource -> TextBuilder
shortName = \case
  Cadrium -> "Cad"
  Erchanite -> "Ern"
  Tellerite -> "Tlr"
  Installation _ -> "Inst"

printCost :: HashMap Resource Double -> TextBuilder
printCost =
  itoList
  >>> map (\(resource, mass) -> Print.float0 mass <> " " <> shortName resource)
  >>> List.intersperse ", "
  >>> fold
