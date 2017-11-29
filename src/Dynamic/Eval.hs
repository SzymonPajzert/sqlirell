module Dynamic.Eval (run) where

import           Control.Arrow
import           Control.Monad.Reader

import           Dynamic.Environment
import           Dynamic.Syntax

import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set

import           Utilities

run :: Expression -> Value
run expression = runReader (eval expression) emptyEnv

isMissing :: Expression -> Bool
isMissing (ValueExpr Missing) = True
isMissing _                   = False

extractEvalString :: Expression -> Maybe String
extractEvalString (ValueExpr (AtomicString string)) = Just string
extractEvalString _                                 = Nothing

eval :: Expression -> EnvReader Value
eval (ValueExpr value) = case value of
  ObjectValue object -> object |> mapM (fmap ValueExpr . eval) |> fmap ObjectValue
  _ -> return value

eval (VariableBinding identifier) = getBinding identifier >>= eval

eval (ComprExpr (ObjectComprehension x y iterator)) = do
  environments <- generateBinds iterator

  let getPair env = (runReader (eval x) env, runReader (eval y) env)

  let extractString (arg, value) = case arg of
        AtomicString ident -> Just (ident, value)
        _                  -> Nothing

  let binds = map getPair environments
        |> mapMaybe extractString
        |> map (\(ident, value) -> (ident, ValueExpr value))
        |> filter (not . isMissing . snd)

  let object = Map.fromList binds
  return $ ObjectValue object

eval (ComprExpr comprehension) = case comprehension of
  ArrayComprehension x iterator -> ArrayValue `fmap` (getArray x iterator)
  BagComprehension x iterator -> (BagValue . Set.fromList) `fmap` (getArray x iterator)
  ObjectComprehension x y iterator -> do
    identifiers <- getArray x iterator
    values <- getArray y iterator

    let validPairs = mapMaybe (\(s, v) -> extractEvalString s >>= (\e -> return (e, v))) (zip identifiers values)

    return $ ObjectValue $ Map.fromList validPairs
  where
    getArray :: Expression -> Iterator -> EnvReader [Expression]
    getArray x iterator = do
      environments <- generateBinds iterator

      return $ environments
        |> map (runReader (eval x))
        |> map ValueExpr
        |> filter (not . isMissing)


evalPair :: (Identifier, Expression) -> EnvReader (Identifier, Value)
evalPair (bind, expr) = do
  value <- eval expr
  return (bind, value)

-- TODO check whether the iterator preserves bindings
generateBinds :: Iterator -> EnvReader [Environment]

generateBinds (ArrayIterator ident expression) = do
  oldEnv <- ask
  evaluated <- eval expression
  let newEnv arrayValue = addBinding ident arrayValue oldEnv

  let toArray = case evaluated of
        ArrayValue array   -> array
        ObjectValue object ->  map snd $ Map.toList object
        BagValue bag       -> Set.toList bag
        _                  -> [] -- no bindings are generated

  toArrayEval <- mapM eval toArray

  toArrayEval
    |> map ValueExpr
    |> map newEnv
    |> return

generateBinds (ObjectIterator idX idY expression) = do
  oldEnv <- ask
  evaluated <- eval expression

  let newEnv (valueX, valueY) = oldEnv
        |> addBinding idX (ValueExpr $ AtomicString valueX)
        |> addBinding idY valueY

  let toObject = case evaluated of
        ObjectValue object -> Map.toList object
        _                  -> [] -- no bindings are generated

  toObjectEval <- mapM evalPair toObject
  toObjectEval
    |> map (second ValueExpr)
    |> map newEnv
    |> return

generateBinds EmptyIterator = do
  oldEnv <- ask
  return [oldEnv]

generateBinds (SequenceIterator firstSeq secondSeq) = do
  firstEnvironments <- generateBinds firstSeq

  let calculateEnvs = runReader (generateBinds secondSeq)

  return $ firstEnvironments >>= calculateEnvs

-- TODO implement
generateBinds (IteratorModifier _ iterator) = generateBinds iterator
