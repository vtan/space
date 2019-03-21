module App.Common.Display where

import App.Prelude

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text.Lazy.Builder.Int as TextBuilder
import qualified Data.Text.Lazy.Builder.RealFloat as TextBuilder

class Display a where
  display :: a -> TextBuilder

instance Display Text where
  display = TextBuilder.fromText

instance Display Int where
  display = TextBuilder.decimal

instance Display Double where
  display = TextBuilder.formatRealFloat TextBuilder.Fixed (Just 2)

fixed :: Int -> Double -> TextBuilder
fixed digits =
  TextBuilder.formatRealFloat TextBuilder.Fixed (Just digits)

normal :: Double -> TextBuilder
normal x =
  let ax = abs x
  in if
    | ax < 1_000 -> fixed 1 x
    | ax < 1_000_000 -> fixed 1 (x / 1_000) <> "k"
    | otherwise -> fixed 1 (x / 1_000_000) <> "M"

int02 :: Int -> TextBuilder
int02 x
  | x < 10 = "0" <> TextBuilder.decimal x
  | otherwise = TextBuilder.decimal x

percent :: Double -> TextBuilder
percent x =
  fixed 0 (100 * x) <> "%"

toText :: TextBuilder -> Text
toText =
  TextBuilder.toLazyText >>> LazyText.toStrict
