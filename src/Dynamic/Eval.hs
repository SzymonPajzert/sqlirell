module Dynamic.Eval (run) where

import Control.Monad.Reader

import Dynamic.Syntax
import Dynamic.Err
import Dynamic.Environment

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

import Utilities ((|>), (|>>))

run :: Expression -> Value
run expression = runReader (eval expression) emptyEnv 

-- check wether to use it
newtype RuntimeError = RuntimeError ()
type RunErr a = Err RuntimeError a

isIdent :: Value -> Bool
isIdent = _
isMissing :: Value -> Bool
isMissing = _


eval :: Expression -> EnvReader Value
eval (ValueExpr value) = return value
eval (VariableBinding identifier) = getBinding identifier
   
eval (ComprExpr (ObjectComprehension x y iterator)) = do
  environments <- generateBinds iterator

  let getPair env = (runReader (eval x) env, runReader (eval y) env)

  let extractString (arg, value) = case arg of
        AtomicString ident -> Just (ident, value)
        _ -> Nothing
  
  let binds = map getPair environments
        |> mapMaybe extractString
        |> filter (not . isMissing . snd) 
  
  let object = Map.fromList binds
  return $ ObjectValue object

eval (ComprExpr comprehension) = case comprehension of
  ArrayComprehension x iterator -> ArrayValue `fmap` (getArray x iterator)
  BagComprehension x iterator -> (BagValue . Set.fromList) `fmap` (getArray x iterator) 
  where
    getArray x iterator = do
      environments <- generateBinds iterator

      return $ environments
        |> map (runReader (eval x))
        |> filter (not . isMissing)
  

-- TODO check whether the iterator preserves bindings
generateBinds :: Iterator -> EnvReader [Environment]

generateBinds (ArrayIterator id expression) = do
  oldEnv <- ask
  evaluated <- eval expression
  let newEnv arrayValue = addBinding id arrayValue oldEnv 

  let toArray = case evaluated of
        ArrayValue array -> array
        ObjectValue object ->  map snd $ Map.toList object
        BagValue bag -> Set.toList bag
        _ -> [] -- no bindings are generated 
  
  return $ map newEnv toArray

generateBinds (ObjectIterator idX idY expression) = do
  oldEnv <- ask
  evaluated <- eval expression
  
  let newEnv (valueX, valueY) = oldEnv
        |> addBinding idX (AtomicString valueX)
        |> addBinding idY (valueY)

  let toObject = case evaluated of
        ObjectValue object -> Map.toList object
        _ -> [] -- no bindings are generated 
  
  return $ map newEnv toObject

generateBinds EmptyIterator = do
  oldEnv <- ask
  return [oldEnv]

generateBinds (SequenceIterator first second) = do
  firstEnvironments <- generateBinds first

  let calculateEnvs firstEnv = runReader (generateBinds second) firstEnv

  return $ firstEnvironments >>= calculateEnvs

-- TODO implement
generateBinds (IteratorModifier _ iterator) = generateBinds iterator
