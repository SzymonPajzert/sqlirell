module Dynamic.Environment (
  Environment,
  EnvReader,
  EnvResult,
  addBinding,
  getBindEnv,
  getBinding,
  emptyEnv,
  fromList) where

import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Maybe
import           Dynamic.Syntax       (Expression (..), Identifier, Value (..))

newtype Bindings = Bindings { getMap :: Map.Map Identifier Expression }

type EnvResult = Expression

instance Show Bindings where
  show (Bindings bindMap) = unlines $ map represent (Map.toList bindMap)
    where represent (var, expr) = var ++ " -> " ++ show expr

newtype Environment = Environment
  { bindings :: Bindings } deriving (Show)

emptyEnv :: Environment
emptyEnv = Environment $ Bindings Map.empty

type EnvReader a = Reader Environment a

getBindEnv :: Identifier -> Environment -> EnvResult
getBindEnv ident env = let
  lookupValue = Map.lookup ident $ getMap $ bindings env
  defaultValue = ValueExpr Missing
  in fromMaybe defaultValue lookupValue

getBinding :: Identifier -> EnvReader EnvResult
getBinding ident = asks (getBindEnv ident)

addBinding :: Identifier -> EnvResult -> Environment -> Environment
addBinding var expr env = env { bindings = Bindings newBindings }
  where newBindings = Map.insert var expr (getMap $ bindings env)

fromList :: [(Identifier, Expression)] -> Environment
fromList = foldl (flip $ uncurry addBinding) emptyEnv

