module App.Model.Mineral where

import App.Prelude

import qualified Control.Lens as Lens

data Mineral = Mineral
  { available :: Int
  , accessibility :: Double
  }
  deriving (Show, Generic, Eq)

nonEmpty :: Lens.Iso' (Maybe Mineral) Mineral
nonEmpty =
  Lens.anon
    Mineral{ available = 0, accessibility = 0 }
    (\Mineral{ available } -> available == 0)
