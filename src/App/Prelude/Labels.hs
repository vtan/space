{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Prelude.Labels where

import Prelude (Functor)
import Data.Generics.Product (HasField, field)
import GHC.OverloadedLabels (IsLabel(..))

instance (HasField field s t a b, Functor f, sft ~ (s -> f t))
  => IsLabel field ((a -> f b) -> sft) where
  fromLabel = field @field