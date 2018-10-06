module App.Update.WidgetLayout where

import App.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.Aeson ((.:?))

data WidgetLayout = WidgetLayout
  { name :: Text
  , xy :: Maybe (V2 Int)
  , wh :: Maybe (V2 Int)
  , layout :: Layout
  , padding :: Maybe Int
  , children :: [WidgetLayout]
  }
  deriving (Show, Generic)

data Layout
  = Fixed
  | Vertical
  | Horizontal
  deriving (Show, Generic)

instance Aeson.FromJSON WidgetLayout where
  parseJSON = \case
    Aeson.Object obj -> do
      name <- obj .:? "name" <&> fromMaybe ""

      xy <- obj .:? "xy" >>= \case
        Just ([x, y] :: [Int]) -> pure $ Just (V2 x y)
        Nothing -> pure Nothing
        Just invalid -> fail ("Invalid vector: " ++ show invalid)

      wh <- obj .:? "wh" >>= \case
        Just ([w, h] :: [Int]) -> pure $ Just (V2 w h)
        Nothing -> pure Nothing
        Just invalid -> fail ("Invalid vector: " ++ show invalid)

      layout <- obj .:? "layout" >>= \case
        Just "fixed" -> pure Fixed
        Just "vertical" -> pure Vertical
        Just "horizontal" -> pure Horizontal
        Nothing -> pure Fixed
        Just invalid -> fail ("Invalid layout: " ++ invalid)

      padding <- obj .:? "padding"
      childList <- obj .:? "children" <&> fromMaybe []
      children <- childList & traverse Aeson.parseJSON

      pure WidgetLayout{..}

    invalid ->
      Aeson.typeMismatch "WidgetLayout" invalid
