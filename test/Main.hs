module Main where

import Eval.EvalTest (tests)

import           Test.HUnit
main :: IO ()
main = runTestTT tests >>= print
