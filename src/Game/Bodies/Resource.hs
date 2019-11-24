module Game.Bodies.Resource where

import GlobalImports

import Game.Common.Display (Display, display)

import qualified Data.HashMap.Strict as HashMap

import Linear ((^-^))

data Resource
  = Resource1
  | Resource2
  deriving (Show, Generic, Eq, Enum, Bounded)

instance Hashable Resource

instance Display Resource where
  display = \case
    Resource1 -> "Resource1"
    Resource2 -> "Resource2"

all :: [Resource]
all = [minBound .. maxBound]

spend :: HashMap Resource Double -> HashMap Resource Double -> Maybe (HashMap Resource Double)
spend resources cost =
  let candidate = HashMap.filter (/= 0) (resources ^-^ cost)
  in
    if GlobalImports.all (> 0) candidate
    then Just candidate
    else Nothing
