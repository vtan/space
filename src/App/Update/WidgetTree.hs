module App.Update.WidgetTree where

import App.Prelude

import qualified App.Rect as Rect
import qualified App.Update.WidgetLayout as WidgetLayout
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import App.Rect (Rect)
import App.Update.WidgetLayout (WidgetLayout(..))
import Control.Lens (Lens')

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
      children = layoutChildren
        & map (\ch@WidgetLayout{ name } -> (name, fromWidgetLayout ch))
        & unzip
        & over _2 (packChildren position layout)
        & uncurry zip
        & HashMap.fromList
        & unpackUnderscoreChildren
      size = wh & fromMaybe (
          foldl' Rect.union Rect.zero (children ^.. folded . #bounds)
            & view #wh
        )
  in WidgetTree
    { bounds = Rect.fromMinSize position size
    , children = children
    }

packChildren :: V2 Int -> WidgetLayout.Layout -> [WidgetTree] -> [WidgetTree]
packChildren origin layout children =
  case layout of
    WidgetLayout.Fixed ->
      children <&> mapTree (#bounds . #xy +~ origin)
    WidgetLayout.Horizontal padding ->
      children
        & foldl' (appendPackedChild _x padding) ([], origin)
        & fst
    WidgetLayout.Vertical padding ->
      children
        & foldl' (appendPackedChild _y padding) ([], origin)
        & fst

appendPackedChild
  :: (forall a. Lens' (V2 a) a)
  -> Int
  -> ([WidgetTree], V2 Int)
  -> WidgetTree
  -> ([WidgetTree], V2 Int)
appendPackedChild axis padding (accChildren, accPosition) ch =
  let diff = accPosition
        & axis .~ (accPosition ^. axis) + (ch ^. #bounds . #xy . axis)
      ch' = ch & mapTree (#bounds . #xy +~ diff)
      increment = padding + ch' ^. #bounds . #wh . axis
  in (accChildren |> ch', accPosition & axis +~ increment)

unpackUnderscoreChildren :: HashMap Text WidgetTree -> HashMap Text WidgetTree
unpackUnderscoreChildren children =
  let cond = Text.isPrefixOf "_"
      unpackedChildren = children ^. ifolded . ifiltered (\i _ -> cond i) . #children
  in (children & HashMap.filterWithKey (\i _ -> not (cond i))) <> unpackedChildren

mapTree :: (WidgetTree -> WidgetTree) -> WidgetTree -> WidgetTree
mapTree f =
  over #children (fmap (mapTree f)) >>> f
