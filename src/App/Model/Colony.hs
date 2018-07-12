module App.Model.Colony where

import App.Prelude

import App.Model.BodyMinerals (Mineral)

data Colony = Colony
  { stockpile :: HashMap Mineral Double 
  , mines :: HashMap Mineral Int
  }
  deriving (Show, Generic)