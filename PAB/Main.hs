{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (void)
import Control.Monad.Freer (interpret)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), fromJSON)
import Data.Default (def)
import Data.Monoid (Last (..))
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)
import Ledger.TimeSlot
import Ledger.Value (TokenName)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Events.Contract (ContractInstanceId)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Prettyprinter (Pretty (..), viaShow)
import RPS
import Wallet.Emulator.Wallet

data RPSContracts = StartGame FirstParams | AnswerToGame SecondParams
  deriving (Show, Eq, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty RPSContracts where
  pretty = viaShow

instance Builtin.HasDefinitions RPSContracts where
  getDefinitions = []
  getSchema = \case
    _ -> Builtin.endpointsToSchemas @GameSchema
  getContract = \case
    StartGame fp -> SomeBuiltin $ firstGame fp
    AnswerToGame sp -> SomeBuiltin $ secondGame sp

handlers :: SimulatorEffectHandlers (Builtin RPSContracts)
handlers =
  Simulator.mkSimulatorHandlers def def $
    interpret (contractHandler Builtin.handleBuiltin)

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
  flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just x)) -> Just x
    _ -> Nothing

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin RPSContracts) "Starting Rock,Paper,Scissors game PAB, webserver on port 9080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2
        stake = 3_000_000
        deadlinePlay = slotToEndPOSIXTime def 5
        deadlineReveal = slotToEndPOSIXTime def 10
        startParams =
          FirstParams
            { fpSecond = mockWalletPaymentPubKeyHash wallet2,
              fpStake = stake,
              fpPlayDeadline = deadlinePlay,
              fpRevealDeadline = deadlineReveal,
              fpChoice = Rock,
              fpNonce = "SECRETWORD"
            }
    cidInit <- Simulator.activateContract wallet1 $ StartGame startParams
    Simulator.waitNSlots 1
    tt' <- waitForLast cidInit

    case tt' of
      Nothing -> Simulator.logString @(Builtin RPSContracts) "thread token no founded" >> Simulator.logString @(Builtin RPSContracts) (show tt') >> shutdown
      Just tt -> do
        Simulator.logString @(Builtin RPSContracts) ("thread token = " ++ show tt)
        let answerParams =
              SecondParams
                { spFirst = mockWalletPaymentPubKeyHash wallet1,
                  spStake = stake,
                  spPlayDeadline = deadlinePlay,
                  spRevealDeadline = deadlineReveal,
                  spChoice = Scissors,
                  spToken = tt
                }
        void $ Simulator.waitNSlots 2
        void $ Simulator.activateContract wallet2 $ AnswerToGame answerParams

        void $ Simulator.waitNSlots 10
        void $ liftIO getLine
        void $ Simulator.waitNSlots 2

        Simulator.logString @(Builtin RPSContracts) "Balances at the end of the simulation"
        b <- Simulator.currentBalances
        Simulator.logBalances @(Builtin RPSContracts) b
        shutdown
