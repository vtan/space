module App.Common.Camera where

import App.Prelude

import App.Common.Rect (Rect(..))

data Camera a b = Camera
  { conversion :: AnIso' a b
  , eyeFrom :: V2 a
  , eyeTo :: V2 b
  , scale :: V2 b
  }
  deriving (Generic)

instance (Show a, Show b) => Show (Camera a b) where
  show Camera{ eyeFrom, scale } = show (eyeFrom, scale)

vectorToScreen :: Num b => Camera a b -> V2 a -> V2 b
vectorToScreen Camera{ conversion, scale } p =
  fmap (view $ cloneIso conversion) p * scale

pointToScreen :: (Num a, Num b) => Camera a b -> V2 a -> V2 b
pointToScreen Camera{ conversion, eyeFrom, eyeTo, scale } p =
  fmap (view $ cloneIso conversion) (p - eyeFrom) * scale + eyeTo

screenToVector :: (Fractional b) => Camera a b -> V2 b -> V2 a
screenToVector Camera{ conversion, scale } p =
  fmap (view $ from conversion) (p / scale)

screenToPoint :: (Num a, Fractional b) => Camera a b -> V2 b -> V2 a
screenToPoint Camera{ conversion, eyeFrom, eyeTo, scale } p =
  fmap (view $ from conversion) ((p - eyeTo) / scale) + eyeFrom

rectToScreen :: (Num a, Num b) => Camera a b -> Rect a -> Rect b
rectToScreen cam (Rect xy wh) =
  Rect (pointToScreen cam xy) (vectorToScreen cam wh)

unscale :: Fractional b => Camera a b -> b -> a
unscale Camera{ conversion, scale } x =
  view (from conversion) (x / (scale ^. _x)) -- hacky, but scale.y should be -scale.x
