module App.Model.Resource where

import App.Prelude

import qualified App.Common.Display as Display
import qualified App.Model.Installation as Installation
import qualified Data.List as List

import App.Model.Installation (Installation(..))

data Resource
  = Cadrium
  | Erchanite
  | Tellerite
  | Installation Installation
  deriving (Show, Generic, Eq)

instance Hashable Resource

all :: [Resource]
all =
  minerals
  ++ [Installation i | i <- Installation.all]

minerals :: [Resource]
minerals = [Cadrium, Erchanite, Tellerite]

print :: Resource -> TextBuilder
print = \case
  Cadrium -> "Cadrium"
  Erchanite -> "Erchanite"
  Tellerite -> "Tellerite"
  Installation i -> Installation.print i <> " Installation"

printShort :: Resource -> TextBuilder
printShort = \case
  Cadrium -> "Cad"
  Erchanite -> "Ern"
  Tellerite -> "Tlr"
  Installation _ -> "Inst"

printCost :: HashMap Resource Double -> TextBuilder
printCost =
  itoList
  >>> map (\(resource, mass) -> Display.float0 mass <> " " <> printShort resource)
  >>> List.intersperse ", "
  >>> fold
