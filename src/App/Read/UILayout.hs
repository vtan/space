module App.Read.UILayout where

import App.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.Aeson ((.:))

data UILayout = UILayout
  { xy :: V2 Int
  , wh :: V2 Int
  , children :: HashMap Text UILayout
  }
  deriving (Show, Generic)

instance Aeson.FromJSON UILayout where
  parseJSON = \case
    Aeson.Object o -> do
      [x, y] :: [Int] <- o .: "xy"
      [w, h] :: [Int] <- o .: "wh"
      children <- o .: "children"
      let xy = V2 x y
          wh = V2 w h
      pure UILayout{..}
    invalid ->
      Aeson.typeMismatch "UILayout" invalid