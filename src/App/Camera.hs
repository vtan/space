module App.Camera where

import App.Prelude

import App.Rect (Rect(..))

data Camera a b = Camera
  { conversion :: AnIso' a b
  , scale :: b
  , translate :: V2 b
  }
  deriving (Generic)

instance Show b => Show (Camera a b) where
  show (Camera _ scale translate) = show (scale, translate)

screenToPoint :: Fractional b => Camera a b -> V2 b -> V2 a
screenToPoint (Camera conversion scale translate) p =
  (p - translate) ^/ scale
    <&> view (from conversion)

pointToScreen :: Num b => Camera a b -> V2 a -> V2 b
pointToScreen (Camera conversion scale translate) (fmap (view $ cloneIso conversion) -> p) =
  scale *^ p + translate

vectorToScreen :: Num b => Camera a b -> V2 a -> V2 b
vectorToScreen (Camera conversion scale _) (fmap (view $ cloneIso conversion) -> p) =
  scale *^ p

rectToScreen :: Num b => Camera a b -> Rect a -> Rect b
rectToScreen cam (Rect xy wh) =
  Rect (pointToScreen cam xy) (vectorToScreen cam wh)