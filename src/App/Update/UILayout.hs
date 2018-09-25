module App.Update.UILayout where

import App.Prelude

import qualified App.Read.UILayout as Read
import qualified App.Rect as Rect
import qualified Data.Text as Text

import App.Rect (Rect)
import App.Read.UILayout (RootLayout(..))

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

fromReadRoot :: Read.RootLayout -> UILayout
fromReadRoot (RootLayout rootChildren) =
  let rootChildren' = fromChildren "root" 0 rootChildren
  in UILayout
    { name = "root"
    , bounds = Rect.fromMinSize 0 0
    , children = rootChildren'
    }
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
