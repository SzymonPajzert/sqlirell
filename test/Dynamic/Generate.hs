module Generate where

import Test.Quickcheck (Gen)
import Dynamic.Syntax

type Env = [Identifier]

exprGen :: GenConfig -> Gen Expression
exprGen = snd . exprGen'

exprGen' :: GenConfig -> Gen (Env, Expression)
exprGen' config =
  frequency [
  (unboundVarRatio config, unboundVar),
  (boundVarRatio config, boundVar),
  (newBoundVarRatio config, newBoundVar),
  (comprRatio config, compr),
  (valueRatio config, value)]

string :: Int -> Gen String
string size = arbitrary :: String

value :: Gen Value
value = sized value'
value' size = frequency [
  (size, ObjectValue `fmap` (object size)),
  (size, BagValue `fmap` (object bag)),
  (1, return $ Null),
  (1, return $ Missing),
  (size / 3, AtomicString `fmap` (string size)),
  (size / 3, AtomicNumber `fmap` (arbitrary :: Integer)),
  (size / 5, AtomicBool `fmap` (arbitrary :: Bool))]

object :: Int -> Gen Object
object size = oneof [flat, nested]
  where flat = _
        nested = _ -- square root or sth (probably read from cfg)
