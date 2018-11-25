module App.Logic.Util where

import App.Prelude

import qualified Data.HashMap.Strict as HashMap

import App.Model.Resource (Resource)
import Linear ((^-^))

payResources :: HashMap Resource Double -> HashMap Resource Double -> Maybe (HashMap Resource Double)
payResources resources cost =
  let candidate = HashMap.filter (/= 0) (resources ^-^ cost)
  in if all (> 0) candidate
  then Just candidate
  else Nothing
