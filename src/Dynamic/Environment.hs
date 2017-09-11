module Dynamic.Environment (
  Environment,
  EnvReader,
  addBinding,
  getBindEnv,
  getBinding,
  emptyEnv) where

import qualified Data.Map as Map
import Dynamic.Syntax (Identifier, Expression(..), Value(..))
import Control.Monad.Reader
import Data.Maybe

newtype Bindings = Bindings { getMap :: (Map.Map Identifier Value) }

type EnvResult = Value

instance Show Bindings where
  show (Bindings bindMap) = unlines $ map represent (Map.toList bindMap)
    where represent (var, exp) = var ++ " -> " ++ (show exp)

data Environment = Environment
  { bindings :: Bindings } deriving (Show)

emptyEnv :: Environment 
emptyEnv = Environment $ Bindings Map.empty

type EnvReader a = Reader Environment a

getBindEnv :: Identifier -> Environment -> EnvResult
getBindEnv ident env = let
  lookupValue = Map.lookup ident $ getMap $ bindings env
  defaultValue = Missing
  in fromMaybe defaultValue lookupValue
  
getBinding :: Identifier -> EnvReader EnvResult
getBinding ident = asks (getBindEnv ident)

addBinding :: Identifier -> EnvResult -> Environment -> Environment
addBinding var exp env = env { bindings = Bindings newBindings }
  where newBindings = Map.insert var exp (getMap $ bindings env)

    
