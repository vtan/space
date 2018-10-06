module App.Update.UILayout where

import App.Prelude

import qualified App.Read.UILayout as Read
import qualified App.Rect as Rect

import App.Rect (Rect)

data UILayout = UILayout
  { name :: Text
  , bounds :: Rect Int
  , children :: HashMap Text UILayout
  }
  deriving (Show, Generic)

empty :: UILayout
empty = UILayout
  { name = ""
  , bounds = Rect.fromMinSize 0 0
  , children = mempty
  }

child :: Text -> UILayout -> UILayout
child childName UILayout{ children } =
  case children ^. at childName of
    Just foundChild -> foundChild
    Nothing -> App.Update.UILayout.empty

fromRead :: Read.UILayout -> UILayout
fromRead = fromRead' "root" 0
  where
    fromRead' :: Text -> V2 Int -> Read.UILayout -> UILayout
    fromRead' accName accOffset Read.UILayout{..} =
      let xy' = accOffset + xy
          children' = fromChildren accName (accOffset + xy) children
      in UILayout
        { name = accName
        , bounds = Rect.fromMinSize xy' wh
        , children = children'
        }

    fromChildren :: Text -> V2 Int -> HashMap Text Read.UILayout -> HashMap Text UILayout
    fromChildren accName accOffset children =
      children & imap (\childName childLayout ->
          let name' = accName <> "." <> childName
          in fromRead' name' accOffset childLayout
        )
