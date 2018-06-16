module App.Camera where

import App.Prelude

import App.Rect (Rect(..))

data Camera a = Camera
  { scale :: a
  , translate :: V2 a
  }
  deriving (Show, Generic, Functor)

screenToPoint :: (Integral a, Fractional b) => Camera a -> V2 a -> V2 b
screenToPoint (Camera scale translate) p =
  (fmap fromIntegral p - fmap fromIntegral translate) / fromIntegral scale

pointToScreen :: Num a => Camera a -> V2 a -> V2 a
pointToScreen (Camera scale translate) p =
  scale *^ p + translate

vectorToScreen :: Num a => Camera a -> V2 a -> V2 a
vectorToScreen (Camera scale _) p =
  scale *^ p

rectToScreen :: Num a => Camera a -> Rect a -> Rect a
rectToScreen cam (Rect xy wh) =
  Rect (pointToScreen cam xy) (vectorToScreen cam wh)

initial :: Camera Int
initial = Camera
  { translate = V2 64 64
  , scale = 48
  }
