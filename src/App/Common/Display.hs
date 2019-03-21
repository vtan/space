module App.Common.Display where

import App.Prelude

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text.Lazy.Builder.Int as TextBuilder
import qualified Data.Text.Lazy.Builder.RealFloat as TextBuilder

text :: Text -> TextBuilder
text =
  TextBuilder.fromText

float0 :: RealFloat a => a -> TextBuilder
float0 =
  TextBuilder.formatRealFloat TextBuilder.Fixed (Just 0)

float2 :: RealFloat a => a -> TextBuilder
float2 =
  TextBuilder.formatRealFloat TextBuilder.Fixed (Just 2)

int :: Integral a => a -> TextBuilder
int =
  TextBuilder.decimal

int02 :: Integral a => a -> TextBuilder
int02 x
  | x < 10 = "0" <> TextBuilder.decimal x
  | otherwise = TextBuilder.decimal x

brackets :: TextBuilder -> TextBuilder
brackets builder =
  "(" <> builder <> ")"

toText :: TextBuilder -> Text
toText =
  TextBuilder.toLazyText >>> LazyText.toStrict
