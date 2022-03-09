module Main where

import Spec.Trace
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Rock,Paper, Scissors tests" $ gameTests tests <> [testGameNotFound] <> [testSecondPlayerNotPlayed]
