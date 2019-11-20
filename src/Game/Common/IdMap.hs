module Game.Common.IdMap where

import GlobalImports

import qualified Data.IntMap as IntMap

import Game.Common.Id (Id(..), getInt)
import Control.Lens (Lens', Traversal')

newtype IdMap i a = IdMap { getIntMap :: IntMap a }
  deriving (Show, Semigroup, Monoid, Foldable, Functor, Traversable)

fromList :: [(Id i, a)] -> IdMap i a
fromList = IdMap . IntMap.fromList . over (mapped . _1) getInt

fromEntities :: (a -> Id i) -> [a] -> IdMap i a
fromEntities f xs = IdMap . IntMap.fromList $ GlobalImports.zip (map (getInt . f) xs) xs

nextId :: IdMap i a -> Id i
nextId (IdMap xs) =
  IntMap.maxViewWithKey xs
    & fmap (fst >>> fst >>> (+ 1))
    & fromMaybe 0
    & Id

keys :: IdMap i a -> [Id i]
keys = getIntMap >>> IntMap.keys >>> map Id

zip :: IdMap i a -> IdMap i b -> IdMap i (a, b)
zip (IdMap xs) (IdMap ys) = IdMap $ IntMap.intersectionWith (,) xs ys

type instance Index (IdMap i a) = Id i
type instance IxValue (IdMap i a) = a

instance Ixed (IdMap i a) where
  ix :: Id i -> Traversal' (IdMap i a) a
  ix (Id n) f (IdMap intMap) = IdMap <$> ix n f intMap

instance At (IdMap i a) where
  at :: Id i -> Lens' (IdMap i a) (Maybe a)
  at (Id n) f (IdMap intMap) = IdMap <$> at n f intMap
