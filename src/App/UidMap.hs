module App.UidMap where

import App.Prelude

import qualified Data.IntMap as IntMap

import App.Uid (Uid(..), getInt)
import Control.Lens (Lens', Traversal')

newtype UidMap a = UidMap { getIntMap :: IntMap a }
  deriving (Show, Semigroup, Monoid, Foldable, Functor, Traversable)

fromEntities :: (a -> Uid a) -> [a] -> UidMap a
fromEntities f xs = UidMap . IntMap.fromList $ zip (map (getInt . f) xs) xs

type instance Index (UidMap a) = Uid a
type instance IxValue (UidMap a) = a

instance Ixed (UidMap a) where
  ix :: Uid a -> Traversal' (UidMap a) a
  ix (Uid n) f (UidMap intMap) = UidMap <$> ix n f intMap

instance At (UidMap a) where
  at :: Uid a -> Lens' (UidMap a) (Maybe a)
  at (Uid n) f (UidMap intMap) = UidMap <$> at n f intMap