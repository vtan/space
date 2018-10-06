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
  , children :: [WidgetLayout]
  }
  deriving (Show, Generic)

data Layout
  = Fixed
  | Vertical { padding :: Int }
  | Horizontal { padding :: Int }
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
        Just "vertical" -> do
          padding <- obj .:? "padding" <&> fromMaybe 0
          pure $ Vertical padding
        Just "horizontal" -> do
          padding <- obj .:? "padding" <&> fromMaybe 0
          pure $ Horizontal padding
        Just "fixed" -> pure Fixed
        Nothing -> pure Fixed
        Just invalid -> fail ("Invalid layout: " ++ invalid)

      childList <- obj .:? "children" <&> fromMaybe []
      children <- childList & traverse Aeson.parseJSON

      pure WidgetLayout{..}

    invalid ->
      Aeson.typeMismatch "WidgetLayout" invalid
