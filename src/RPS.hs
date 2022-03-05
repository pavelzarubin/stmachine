{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RPS where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import qualified Data.OpenApi as OpenApi
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx
import Playground.Contract (ToSchema)
import Plutus.Contract as Contract
import Plutus.Contract.StateMachine
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Eq, Semigroup (..), Show (..), String)

data GameTurns = Rock | Paper | Scissors deriving (Show, Generic, FromJSON, ToJSON, ToSchema, OpenApi.ToSchema)

PlutusTx.unstableMakeIsData ''GameTurns

data Game = Game
  { gFirstPlayer :: PaymentPubKeyHash,
    gSecondPlayer :: PaymentPubKeyHash,
    gStake :: Integer,
    gPlayDeadline :: POSIXTime,
    gRevealDeadline :: POSIXTime,
    gToken :: !ThreadToken
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, Prelude.Eq)

PlutusTx.makeLift ''Game

instance PlutusTx.Prelude.Eq GameTurns where
  {-# INLINEABLE (==) #-}
  Rock == Rock = True
  Paper == Paper = True
  Scissors == Scissors = True
  _ == _ = False

data GameDatum = GameDatum BuiltinByteString (Maybe GameTurns) | Finish deriving (Show)

instance PlutusTx.Prelude.Eq GameDatum where
  {-# INLINEABLE (==) #-}
  GameDatum bs1 gt1 == GameDatum bs2 gt2 = (bs1 == bs2) && (gt1 == gt2)
  Finish == Finish = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameTurns | Reveal BuiltinByteString | ClaimFirst | ClaimSecond deriving (Show)

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINEABLE lovelaceInt #-}
lovelaceInt :: Value -> Integer
lovelaceInt = getLovelace . fromValue

{-# INLINEABLE transitionGame #-}
transitionGame :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transitionGame game st red = case (stateValue st, stateData st, red) of
  (v, GameDatum bs Nothing, Play t)
    | lovelaceInt v == gStake game ->
      Just
        ( Constraints.mustBeSignedBy (gSecondPlayer game)
            <> Constraints.mustValidateIn (to $ gPlayDeadline game),
          State (GameDatum bs (Just t)) (Ada.lovelaceValueOf $ 2 * gStake game)
        )
  (v, GameDatum _ (Just _), Reveal _)
    | lovelaceInt v == (2 * gStake game) ->
      Just
        ( Constraints.mustBeSignedBy (gFirstPlayer game)
            <> Constraints.mustValidateIn (to $ gRevealDeadline game),
          State Finish mempty
        )
  (v, GameDatum _ Nothing, ClaimFirst)
    | lovelaceInt v == gStake game ->
      Just
        ( Constraints.mustBeSignedBy (gFirstPlayer game)
            <> Constraints.mustValidateIn (from $ 1 + gPlayDeadline game),
          State Finish mempty
        )
  (v, GameDatum _ (Just _), ClaimSecond)
    | lovelaceInt v == (2 * gStake game) ->
      Just
        ( Constraints.mustBeSignedBy (gSecondPlayer game)
            <> Constraints.mustValidateIn (from $ 1 + gRevealDeadline game),
          State Finish mempty
        )
  _ -> Nothing

{-# INLINEABLE finish #-}
finish :: GameDatum -> Bool
finish Finish = True
finish _ = False

{-# INLINEABLE check #-}
check :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check rock paper scissors (GameDatum bs (Just c)) (Reveal nonce) _ =
  traceIfFalse "bad nonce" check'
  where
    check' =
      sha2_256
        ( nonce `appendByteString` case c of
            Rock -> paper
            Paper -> scissors
            Scissors -> rock
        )
        == bs
check _ _ _ _ _ _ = True

{-# INLINEABLE gameSTM #-}
gameSTM :: Game -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> StateMachine GameDatum GameRedeemer
gameSTM game rock paper scissors =
  StateMachine
    { smTransition = transitionGame game,
      smFinal = finish,
      smCheck = RPS.check rock paper scissors,
      smThreadToken = Just $ gToken game
    }

{-# INLINEABLE turnToBs #-}
turnToBs :: GameTurns -> BuiltinByteString
turnToBs Rock = "Rock"
turnToBs Paper = "Paper"
turnToBs Scissors = "Scissors"

gameSTM' :: Game -> StateMachine GameDatum GameRedeemer
gameSTM' game = gameSTM game bsRock bsPaper bsScissors

{-# INLINEABLE mkGameValidator #-}
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game rock paper scissors = mkValidator $ gameSTM game rock paper scissors

bsRock, bsPaper, bsScissors :: BuiltinByteString
bsRock = "0"
bsPaper = "1"
bsScissors = "2"

type Gaming = StateMachine GameDatum GameRedeemer

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game =
  Scripts.mkTypedValidator @Gaming
    ( $$(PlutusTx.compile [||mkGameValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsRock
        `PlutusTx.applyCode` PlutusTx.liftCode bsPaper
        `PlutusTx.applyCode` PlutusTx.liftCode bsScissors
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Address
gameAddress = scriptAddress . gameValidator

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameSTM' game) (typedGameValidator game)

data FirstParams = FirstParams
  { fpSecond :: PaymentPubKeyHash,
    fpStake :: Integer,
    fpPlayDeadline :: POSIXTime,
    fpRevealDeadline :: POSIXTime,
    fpNonce :: BuiltinByteString,
    fpChoice :: GameTurns
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, OpenApi.ToSchema)

win :: GameTurns -> GameTurns -> Bool
win player1 player2 = case (player1, player2) of
  (Rock, Scissors) -> True
  (Paper, Rock) -> True
  (Scissors, Paper) -> True
  _ -> False

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError (pack . show)

firstGame :: FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame FirstParams {..} = do
  pkh <- Contract.ownPaymentPubKeyHash
  tt <- mapError' getThreadToken
  let game =
        Game
          { gFirstPlayer = pkh,
            gSecondPlayer = fpSecond,
            gPlayDeadline = fpPlayDeadline,
            gRevealDeadline = fpRevealDeadline,
            gStake = fpStake,
            gToken = tt
          }
      client = gameClient game
      v = Ada.lovelaceValueOf fpStake
      bs =
        sha2_256
          ( fpNonce `appendByteString` case fpChoice of
              Rock -> "0"
              Paper -> "1"
              Scissors -> "2"
          )
  void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
  logInfo @String $ "made first move: " ++ show fpChoice
  tell $ Last $ Just tt

  void $ awaitTime fpPlayDeadline
  void $ waitNSlots 1

  m <- mapError' $ getOnChainState client
  case m of
    Nothing -> throwError "game output not found"
    Just (o, _) -> case tyTxOutData $ ocsTxOut o of
      GameDatum _ Nothing -> do
        logInfo @String "second player didn't play"
        void $ mapError' $ runStep client ClaimFirst
      GameDatum _ (Just t') | win fpChoice t' -> do
        logInfo @String "second player played and lost"
        void $ mapError' $ runStep client $ Reveal fpNonce
        logInfo @String "first player won and revealed"
      _ -> logInfo @String "second player won"

data SecondParams = SecondParams
  { spFirst :: PaymentPubKeyHash,
    spStake :: Integer,
    spPlayDeadline :: POSIXTime,
    spRevealDeadline :: POSIXTime,
    spChoice :: GameTurns,
    spToken :: ThreadToken
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, OpenApi.ToSchema)

secondGame :: SecondParams -> Contract w s Text ()
secondGame SecondParams {..} = do
  pkh <- Contract.ownPaymentPubKeyHash
  let game =
        Game
          { gFirstPlayer = spFirst,
            gSecondPlayer = pkh,
            gPlayDeadline = spPlayDeadline,
            gRevealDeadline = spRevealDeadline,
            gStake = spStake,
            gToken = spToken
          }
      client = gameClient game
  m <- mapError' $ getOnChainState client
  case m of
    Nothing -> logInfo @String "game not found"
    Just (o, _) -> case tyTxOutData $ ocsTxOut o of
      GameDatum _ Nothing -> do
        logInfo @String "game founded"
        void $ mapError' $ runStep client $ Play spChoice
        logInfo @String $ "second player move: " ++ show spChoice

        void $ awaitTime spRevealDeadline
        void $ waitNSlots 1

        m' <- mapError' $ getOnChainState client
        case m' of
          Nothing -> logInfo @String "first player won"
          Just _ -> do
            logInfo @String "first player didn't reveal"
            void $ mapError' $ runStep client ClaimSecond
            logInfo @String "second player won"
      _ -> throwError "unexpected datum"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = selectList [first, second] >> endpoints
  where
    first = endpoint @"first" firstGame
    second = endpoint @"second" secondGame
