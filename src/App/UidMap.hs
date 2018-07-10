module App.UidMap where

import App.Prelude

import qualified Data.IntMap as IntMap

import App.Uid (Uid(..), getInt)
import Control.Lens (Lens', Traversal')

newtype UidMap i a = UidMap { getIntMap :: IntMap a }
  deriving (Show, Semigroup, Monoid, Foldable, Functor, Traversable)

fromList :: [(Uid i, a)] -> UidMap i a
fromList = UidMap . IntMap.fromList . over (mapped . _1) getInt

fromEntities :: (a -> Uid i) -> [a] -> UidMap i a
fromEntities f xs = UidMap . IntMap.fromList $ zip (map (getInt . f) xs) xs


type instance Index (UidMap i a) = Uid i
type instance IxValue (UidMap i a) = a

instance Ixed (UidMap i a) where
  ix :: Uid i -> Traversal' (UidMap i a) a
  ix (Uid n) f (UidMap intMap) = UidMap <$> ix n f intMap

instance At (UidMap i a) where
  at :: Uid i -> Lens' (UidMap i a) (Maybe a)
  at (Uid n) f (UidMap intMap) = UidMap <$> at n f intMap