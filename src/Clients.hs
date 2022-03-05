{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import Data.Text
import GHC.Generics
import Ledger
import Ledger.TimeSlot
import Network.HTTP.Req
import Plutus.Contract.StateMachine
import Plutus.PAB.Webserver.Types
import RPS
import Wallet.Emulator.Wallet

data ActivateContractParams t = ActivateContractParams
  { contents :: t,
    tag :: String
  }
  deriving (FromJSON, ToJSON, Generic)

startTest :: IO ()
startTest = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson testStartInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "game started" ++ (show response)
        else "error starting"

testStartInstance :: ContractActivationArgs (ActivateContractParams FirstParams)
testStartInstance = ContractActivationArgs {caID = ActivateContractParams fp "StartGame", caWallet = Just (knownWallet 3)}
  where
    fp =
      FirstParams
        { fpSecond = mockWalletPaymentPubKeyHash (knownWallet 4),
          fpStake = 5_000_000,
          fpPlayDeadline = slotToEndPOSIXTime def 60,
          fpRevealDeadline = slotToEndPOSIXTime def 120,
          fpChoice = Paper,
          fpNonce = "something"
        }

answerTest :: IO ()
answerTest = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson testAnswerInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "game answered" ++ (show response)
        else "error starting"

testAnswerInstance :: ContractActivationArgs (ActivateContractParams SecondParams)
testAnswerInstance = ContractActivationArgs {caID = ActivateContractParams fp "AnswerToGame", caWallet = Just (knownWallet 4)}
  where
    fp =
      SecondParams
        { spFirst = mockWalletPaymentPubKeyHash (knownWallet 3),
          spStake = 5_000_000,
          spPlayDeadline = slotToEndPOSIXTime def 30,
          spRevealDeadline = slotToEndPOSIXTime def 60,
          spChoice = Scissors,
          spToken = tt
        }

tt :: ThreadToken
tt =
  ThreadToken
    ( TxOutRef
        { txOutRefId = "8cc29ae699b906e31bbb8b3e55eea772477cd9aeb7154ce8526752f6f986a31a",
          txOutRefIdx = 0
        }
    )
    "089e1938e499e8f7217e58b6535e88fa59f35b8135c27b3b3b724ea6"
