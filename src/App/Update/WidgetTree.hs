module App.Update.WidgetTree where

import App.Prelude

import qualified App.Rect as Rect
import qualified App.Update.WidgetLayout as WidgetLayout
import qualified Data.HashMap.Strict as HashMap

import App.Rect (Rect)
import App.Update.WidgetLayout (WidgetLayout(..))

data WidgetTree = WidgetTree
  { bounds :: Rect Int
  , children :: HashMap Text WidgetTree
  }
  deriving (Show, Generic)

empty :: WidgetTree
empty = WidgetTree
  { bounds = Rect.zero
  , children = mempty
  }

child :: Text -> WidgetTree -> WidgetTree
child childName WidgetTree{ children } =
  case children ^. at childName of
    Just foundChild -> foundChild
    Nothing -> App.Update.WidgetTree.empty

fromWidgetLayout :: WidgetLayout -> WidgetTree
fromWidgetLayout WidgetLayout{ children = layoutChildren, xy, wh, layout } =
  let position = xy & fromMaybe 0
      size = wh & fromMaybe 0
      children = layoutChildren
        & map (\ch@WidgetLayout{ name } -> (name, fromWidgetLayout ch))
        & unzip
        & over _2 (packChildren position layout)
        & uncurry zip
        & HashMap.fromList
  in WidgetTree
    { bounds = Rect.fromMinSize position size
    , children = children
    }

packChildren :: V2 Int -> WidgetLayout.Layout -> [WidgetTree] -> [WidgetTree]
packChildren origin layout children =
  case layout of
    WidgetLayout.Fixed ->
      children & mapTree (#bounds . #xy +~ origin)

mapTree :: Functor f => (WidgetTree -> WidgetTree) -> f WidgetTree -> f WidgetTree
mapTree f =
  fmap (over #children (mapTree f) >>> f)
