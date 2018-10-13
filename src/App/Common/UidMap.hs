module App.Common.UidMap where

import App.Prelude

import qualified Data.IntMap as IntMap

import App.Common.Uid (Uid(..), getInt)
import Control.Lens (Lens', Traversal')

newtype UidMap i a = UidMap { getIntMap :: IntMap a }
  deriving (Show, Semigroup, Monoid, Foldable, Functor, Traversable)

fromList :: [(Uid i, a)] -> UidMap i a
fromList = UidMap . IntMap.fromList . over (mapped . _1) getInt

fromEntities :: (a -> Uid i) -> [a] -> UidMap i a
fromEntities f xs = UidMap . IntMap.fromList $ App.Prelude.zip (map (getInt . f) xs) xs

nextUid :: UidMap i a -> Uid i
nextUid (UidMap xs) =
  IntMap.maxViewWithKey xs
    & fmap (fst >>> fst >>> (+ 1))
    & fromMaybe 0
    & Uid

keys :: UidMap i a -> [Uid i]
keys = getIntMap >>> IntMap.keys >>> map Uid

zip :: UidMap i a -> UidMap i b -> UidMap i (a, b)
zip (UidMap xs) (UidMap ys) = UidMap $ IntMap.intersectionWith (,) xs ys

type instance Index (UidMap i a) = Uid i
type instance IxValue (UidMap i a) = a

instance Ixed (UidMap i a) where
  ix :: Uid i -> Traversal' (UidMap i a) a
  ix (Uid n) f (UidMap intMap) = UidMap <$> ix n f intMap

instance At (UidMap i a) where
  at :: Uid i -> Lens' (UidMap i a) (Maybe a)
  at (Uid n) f (UidMap intMap) = UidMap <$> at n f intMap
