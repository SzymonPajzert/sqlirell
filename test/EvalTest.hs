module Main where

import qualified Data.Map as Map

import Test.HUnit
import Dynamic.Syntax
import Dynamic.Eval

data EvalTestInstance = EvalTestInstance
  { description :: String
  , expression :: Expression
  , value :: Value
  }

assertEval :: EvalTestInstance -> Test
assertEval testInstance = do
  let evaluated = run $ expression testInstance
  TestCase $ assertEqual (description testInstance) (value testInstance) evaluated

tests :: Test
tests = TestList $ map assertEval literalTests

main :: IO ()
main = do
  runTestTT tests >>= (putStrLn . show) 

literalTests :: Test
literalTests = TestList $ map (assertEval . getInstance) descriptions
  where
    getInstance (desc, literal) = let
      newDesc = desc ++ "'s literal"
      in EvalTestInstance newDesc (ValueExpr literal) literal

    stringValue = ValueExpr $ AtomicString "string"
    integerValue = ValueExpr $ AtomicNumber 42
    booleanValue = ValueExpr $ AtomicBool True

    descriptions = [
      ("string", stringValue),
      ("integer", integerValue),
      ("boolean", booleanValue),
      ("null", Null),
      ("missing", Missing)]
      ++ objects
      ++ bags
      ++ arrays

    objects = map (\(desc, obj) -> (desc, ObjectValue obj)) [
      ("empty object", Map.empty),
      ("singleton object", Map.fromList [("a", stringValue)]),
      ("pair object", Map.fromList [("a", stringValue), ("b", Null)])]

    -- TODO
    bags = []
    arrays = [] 


iteratorSequenceTest :: Test
iteratorSequenceTest = TestCase $ assertEval testCase
  where
    testCase = EvalTestInstance "iterator sequence" sourceExpr value
    value = ArrayValue [objectInt 1 3, objectInt 1 4, objectInt 2 3, objectInt 2 4]
    sourceExpr = ComprExpr ArrayExpression expr seqIterator
    expr = object (VariableBinding "a") (VariableBinding "b")
    seqIterator = SequenceIterator firstIterator secondIterator
    firstIterator = ArrayIterator "a" (ArrayValue $ map AtomicNumber [1,2])
    secondIterator = ArrayIterator "b" (ArrayValue $ map AtomicNumber [3,4])

    objectInt x y = ObjectValue $ Map.fromList [
      ("x", ValueExpr $ AtomicNumber x), ("y", ValueExpr $ AtomicNumber y)]
    object x y = ObjectValue $ Map.fromList [("x", x), ("y", y)]
