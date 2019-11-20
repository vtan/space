module Core.Common.Rect where

import GlobalImports

import qualified Linear.Affine as Lin
import qualified SDL

data Rect a = Rect
  { xy :: V2 a
  , wh :: V2 a
  }
  deriving (Show, Generic, Functor)

zero :: Num a => Rect a
zero = Rect 0 0

fromMinSize :: V2 a -> V2 a -> Rect a
fromMinSize = Rect

fromCenterRadius :: Num a => V2 a -> V2 a -> Rect a
fromCenterRadius c r = Rect (c - r) (2 *^ r)

center :: Fractional a => Rect a -> V2 a
center (Rect xy wh) = xy + wh / 2

contains :: (Num a, Ord a) => Rect a -> V2 a -> Bool
contains (Rect (V2 x y) (V2 w h)) (V2 px py) =
  px >= x && px < x + w && py >= y && py < y + h

containsRect :: (Num a, Ord a) => Rect a -> Rect a -> Bool
containsRect (Rect (V2 x1 y1) (V2 w1 h1)) (Rect (V2 x2 y2) (V2 w2 h2)) =
  x2 >= x1 && x2 + w2 <= x1 + w1 && y2 >= y1 && y2 + h2 <= y1 + h1

intersects :: (Num a, Ord a) => Rect a -> Rect a -> Bool
intersects (Rect axy@(V2 ax ay) awh) (Rect bxy@(V2 bx by) bwh) =
  let V2 ax' ay' = axy + awh
      V2 bx' by' = bxy + bwh
  in ax < bx' && bx < ax' && ay < by' && by < ay'

union :: (Num a, Ord a) => Rect a -> Rect a -> Rect a
union (Rect axy@(V2 ax ay) awh) (Rect bxy@(V2 bx by) bwh) =
  let V2 ax' ay' = axy + awh
      V2 bx' by' = bxy + bwh
      x = min ax bx
      y = min ay by
      w = max ax' bx' - x
      h = max ay' by' - y
  in Rect (V2 x y) (V2 w h)

toSdl :: Integral a => Rect a -> SDL.Rectangle CInt
toSdl (Rect xy wh) = SDL.Rectangle (Lin.P $ fromIntegral <$> xy) (fromIntegral <$> wh)
