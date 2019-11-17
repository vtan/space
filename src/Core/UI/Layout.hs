module Core.UI.Layout where

import App.Prelude

import App.Common.Rect (Rect(..))
import Core.UI.UI (UIComponent, UIContext(..))

import Data.Bifunctor (bimap)

data Constrained a
  = Sized Double a
  | Stretched a
  deriving (Generic)

vertical :: [Constrained (UIComponent a)] -> UIComponent a
vertical children = do
  UIContext{ cursor } <- ask
  let
    Rect (V2 left top) (V2 width height) = cursor
    sizeToDistribute = height

    (totalGivenSize, stretchedCount) = children
      & map (\case
          Sized size _ -> Left size
          _ -> Right ()
        )
      & partitionEithers
      & bimap sum (fromIntegral . length)
    stretchedSize =
      if stretchedCount == 0
      then 0
      else (sizeToDistribute - totalGivenSize) / stretchedCount

  (_, stateChange) <- foldlM
    ( \(usedSize, stateChange) constraintedChild ->
        let
          (size, child) = case constraintedChild of
            Sized s c -> (s, c)
            Stretched c -> (stretchedSize, c)
          localCursor = Rect (V2 left (top + usedSize)) (V2 width size)
          usedSize' = usedSize + size
        in
          local (set #cursor localCursor) child
            & fmap (<> stateChange)
            & fmap (usedSize', )
    )
    (0, mempty)
    children

  pure stateChange
