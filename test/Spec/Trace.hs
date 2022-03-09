{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Trace where

import Control.Lens ((&), (.~))
import Control.Monad.Freer.Extras as Extras (logInfo)
import Data.Default (Default (def))
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Ledger
import Ledger.Ada (lovelaceValueOf)
import Ledger.TimeSlot (slotToEndPOSIXTime)
import Plutus.Contract.StateMachine (ThreadToken (ThreadToken))
import Plutus.Contract.Test
  ( TracePredicate,
    checkPredicateOptions,
    defaultCheckOptions,
    emulatorConfig,
    knownWallet,
    mockWalletPaymentPubKeyHash,
    walletFundsChange,
    (.&&.),
  )
import Plutus.Trace
  ( ContractHandle,
    EmulatorConfig,
    EmulatorRuntimeError (GenericError),
    EmulatorTrace,
    activateContractWallet,
    callEndpoint,
    initialChainState,
    observableState,
    throwError,
    waitNSlots,
  )
import RPS
  ( FirstParams
      ( FirstParams,
        fpChoice,
        fpNonce,
        fpPlayDeadline,
        fpRevealDeadline,
        fpSecond,
        fpStake
      ),
    GameSchema,
    GameTurns (..),
    SecondParams
      ( SecondParams,
        spChoice,
        spFirst,
        spPlayDeadline,
        spRevealDeadline,
        spStake,
        spToken
      ),
    endpoints,
  )
import Test.Tasty (TestTree)

{- r p
   p r
   r s
   s r
   s p
   p s-}

emuConf :: EmulatorConfig
emuConf =
  def & initialChainState
    .~ Left
      ( Map.fromList
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

gameTests :: [(String, TracePredicate, GameTurns, GameTurns)] -> [TestTree]
gameTests ((message, changes, p1, p2) : xs) =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConf)
    message
    changes
    (testTrace p1 p2) :
  gameTests xs
gameTests [] = []

winFirst :: TracePredicate
winFirst = walletFundsChange (knownWallet 1) (lovelaceValueOf 3_000_000) .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf (-3_000_000))

winSecond :: TracePredicate
winSecond = walletFundsChange (knownWallet 2) (lovelaceValueOf 3_000_000) .&&. walletFundsChange (knownWallet 1) (lovelaceValueOf (-3_000_000))

draw :: TracePredicate
draw = walletFundsChange (knownWallet 2) (lovelaceValueOf 0) .&&. walletFundsChange (knownWallet 1) (lovelaceValueOf 0)

tests :: [(String, TracePredicate, GameTurns, GameTurns)]
tests =
  [ ("First player move Rock, second Scissors. First player win.", winFirst, Rock, Scissors),
    ("First player move Rock, second Paper. Second player win.", winSecond, Rock, Paper),
    ("First player move Rock, second Rock. Draw.", draw, Rock, Rock),
    ("First player move Paper, second Rock. First player win.", winFirst, Paper, Rock),
    ("First player move Paper, second Scissors. Second player win.", winSecond, Paper, Scissors),
    ("First player move Paper, second Paper. Draw.", draw, Paper, Paper),
    ("First player move Scissors, second Paper. First player win.", winFirst, Scissors, Paper),
    ("First player move Scissors, second Rock. Second player win.", winSecond, Scissors, Rock),
    ("First player move Scissors, second Scissors. Draw.", draw, Scissors, Scissors)
  ]

-----------------------------------------------------------------------------------------------------------------

traceGameNotFound :: EmulatorTrace ()
traceGameNotFound = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let pkh2 = mockWalletPaymentPubKeyHash (knownWallet 2)
      stake = 3_000_000
      deadlinePlay = slotToEndPOSIXTime def 5
      deadlineReveal = slotToEndPOSIXTime def 10
      tt = ThreadToken (TxOutRef "aa" 123) "aa"

  let sp =
        SecondParams
          { spFirst = pkh2,
            spStake = stake,
            spPlayDeadline = deadlinePlay,
            spRevealDeadline = deadlineReveal,
            spChoice = Paper,
            spToken = tt
          }
  void $ waitNSlots 3
  callEndpoint @"second" h1 sp

  void $ waitNSlots 10

  Extras.logInfo @String "TRACE ENDED"

testGameNotFound :: TestTree
testGameNotFound =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConf)
    "GAME NOT FOUND"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    traceGameNotFound

{-runTrace :: IO ()
runTrace = runEmulatorTraceIO' def emuConf traceGameNotFound
-}
------------------------------------------------------------------

traceSecondPlayerNotPlayed :: EmulatorTrace ()
traceSecondPlayerNotPlayed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  let pkh2 = mockWalletPaymentPubKeyHash (knownWallet 2)
      stake = 3_000_000
      deadlinePlay = slotToEndPOSIXTime def 5
      deadlineReveal = slotToEndPOSIXTime def 10

      fp =
        FirstParams
          { fpSecond = pkh2,
            fpStake = stake,
            fpPlayDeadline = deadlinePlay,
            fpRevealDeadline = deadlineReveal,
            fpChoice = Rock,
            fpNonce = ""
          }
  callEndpoint @"first" h1 fp
  void $ waitNSlots 15
  Extras.logInfo @String "TRACE ENDED"

testSecondPlayerNotPlayed :: TestTree
testSecondPlayerNotPlayed =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConf)
    "Second player not play"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    traceSecondPlayerNotPlayed
