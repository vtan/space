module App.Common.Print where

import App.Prelude

import qualified Data.Text.Lazy.Builder.Int as TextBuilder
import qualified Data.Text.Lazy.Builder.RealFloat as TextBuilder

float0 :: RealFloat a => a -> TextBuilder
float0 =
  TextBuilder.formatRealFloat TextBuilder.Fixed (Just 0)

float2 :: RealFloat a => a -> TextBuilder
float2 =
  TextBuilder.formatRealFloat TextBuilder.Fixed (Just 2)

int :: Integral a => a -> TextBuilder
int =
  TextBuilder.decimal

brackets :: TextBuilder -> TextBuilder
brackets builder =
  "(" <> builder <> ")"
