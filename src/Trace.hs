{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Trace where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Ledger.Ada
import Ledger.TimeSlot
import Plutus.Contract.StateMachine
import Plutus.Trace
import RPS
import Wallet.Emulator.Wallet

runTestTrace :: IO ()
runTestTrace = runEmulatorTraceIO' def emuConf (testTrace Rock Scissors)

emuConf :: EmulatorConfig
emuConf =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 10_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

testTrace :: GameTurns -> GameTurns -> EmulatorTrace ()
testTrace player1 player2 = do
  Extras.logInfo $ "FIRST PLAYER MOVE: " ++ show player1 ++ ". SECOND PLAYER MOVE: " ++ show player2

  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let pkh1 = mockWalletPaymentPubKeyHash (knownWallet 1)
      pkh2 = mockWalletPaymentPubKeyHash (knownWallet 2)
      stake = 3_000_000
      deadlinePlay = slotToEndPOSIXTime def 5
      deadlineReveal = slotToEndPOSIXTime def 10

      fp =
        FirstParams
          { fpSecond = pkh2,
            fpStake = stake,
            fpPlayDeadline = deadlinePlay,
            fpRevealDeadline = deadlineReveal,
            fpChoice = player1,
            fpNonce = ""
          }
  callEndpoint @"first" h1 fp
  tt <- getTT h1

  let sp =
        SecondParams
          { spFirst = pkh1,
            spStake = stake,
            spPlayDeadline = deadlinePlay,
            spRevealDeadline = deadlineReveal,
            spChoice = player2,
            spToken = tt
          }
  void $ waitNSlots 3
  callEndpoint @"second" h2 sp

  void $ waitNSlots 10
  Extras.logInfo @String "TRACE ENDED"
  where
    getTT :: ContractHandle (Last ThreadToken) GameSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
      void $ waitNSlots 1
      m <- observableState h
      case m of
        Last Nothing -> throwError $ GenericError "GAME NOT FOUND"
        Last (Just tt) -> Extras.logInfo @String ("GAME FOUNDED" ++ show tt) >> return tt
