module App.Logic.Util where

import App.Prelude

import App.Model.Colony (Colony)
import App.Model.Resource (Resource)

payResources :: HashMap Resource Double -> Colony -> Maybe Colony
payResources cost colony =
  cost & ifoldlM
    (\resource colonyAcc costQty ->
      let (remainingQty, colony') = colonyAcc & #stockpile . at resource . non 0 <-~ costQty
      in if remainingQty >= 0
        then Just colony'
        else Nothing
    )
    colony
