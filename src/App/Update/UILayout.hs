module App.Update.UILayout where

import App.Prelude

import qualified App.Read.UILayout as Read
import qualified App.Rect as Rect
import qualified Data.Text as Text

import App.Rect (Rect)

data UILayout = UILayout
  { name :: Text
  , bounds :: Rect Int
  , children :: HashMap Text UILayout
  }
  deriving (Show, Generic)

child :: Text -> UILayout -> UILayout
child childName UILayout{ name, children } =
  case children ^. at childName of
    Just foundChild -> foundChild
    Nothing -> error . Text.unpack $ Text.unwords ["UI layout", name, "has no child", childName]

fromRead :: Read.UILayout -> UILayout
fromRead = fromRead' "" 0
  where
    fromRead' :: Text -> V2 Int -> Read.UILayout -> UILayout
    fromRead' accName accOffset Read.UILayout{..} =
      let
          xy' = accOffset + xy
          children' = children & imap (\childName childLayout ->
              let name' = accName <> "." <> childName
              in fromRead' name' xy childLayout
            )
      in UILayout
        { name = accName
        , bounds = Rect.fromMinSize xy' wh
        , children = children'
        }
