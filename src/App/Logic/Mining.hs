module App.Logic.Mining where

import App.Prelude

import qualified App.Model.Installation as Installation
import qualified App.Model.Mineral as Mineral
import qualified Data.HashMap.Strict as HashMap

import App.Common.Uid (Uid(..))
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))
import App.Model.Resource (Resource)

mine :: Uid Body -> GameState -> GameState
mine bodyUid gs@GameState{ colonies, bodyMinerals } =
  fromMaybe gs $ do
    colony <- colonies ^. at bodyUid
    minerals <- bodyMinerals ^. at bodyUid
    let mineralsMinedQty =
          HashMap.intersectionWith (,)
            minerals
            (dailyMined colony minerals)
    pure $
      mineralsMinedQty & ifoldl'
        (\resource gs' (Mineral{ available }, minedQty) ->
          let actualMinedQty = min minedQty available
          in gs'
            & #bodyMinerals . at bodyUid . _Just . at resource . Mineral.nonEmpty . #available -~ actualMinedQty
            & #colonies . at bodyUid . _Just . #stockpile . at resource . non 0 +~ actualMinedQty
        )
        gs

changeMiningPriority :: Resource -> Int -> Colony -> GameState -> GameState
changeMiningPriority resource diff Colony{ bodyUid } gs =
  gs & #colonies . at bodyUid . _Just . #miningPriorities . at resource . non 0 %~ \prio ->
    max 0 (prio + diff)

dailyMined :: Colony -> HashMap Resource Mineral -> HashMap Resource Double
dailyMined colony@Colony{ miningPriorities } minerals =
  let totalPrio =
        HashMap.intersectionWith
          (\prio _nonZeroQty -> prio) miningPriorities minerals
        & sum
        & fromIntegral
  in minerals & imap (\resource Mineral{ accessibility } ->
    case miningPriorities ^. at resource of
      Nothing -> 0
      Just prio ->
        let weight = fromIntegral prio / totalPrio
        in accessibility * weight * dailyMinedAtFullAccessibility colony
  )

dailyMinedAtFullAccessibility :: Colony -> Double
dailyMinedAtFullAccessibility Colony{ installations } =
  let mines = installations ^. at Installation.Mine . non 0
  in mines * 0.1
