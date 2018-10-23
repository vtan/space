module App.Common.Queue where

import App.Prelude

import qualified Control.Lens as Lens

update :: Int -> (a -> Maybe a) -> [a] -> Maybe ([a], a)
update i f xs = do
  let (before, rest) = splitAt i xs
  (this, after) <- Lens.uncons rest
  Just
    ( case f this of
        Just newThis -> before ++ (newThis : after)
        Nothing -> before ++ after
    , this
    )

moveUp :: Int -> [a] -> Maybe [a]
moveUp i xs = do
  let (before, rest) = splitAt i xs
  (prefix, elemToSwap) <- Lens.unsnoc before
  (this, suffix) <- Lens.uncons rest
  Just (prefix ++ [this, elemToSwap] ++ suffix)

moveDown :: Int -> [a] -> Maybe [a]
moveDown i xs = do
  let (rest, after) = splitAt (i + 1) xs
  (prefix, this) <- Lens.unsnoc rest
  (elemToSwap, suffix) <- Lens.uncons after
  Just (prefix ++ [elemToSwap, this] ++ suffix)
