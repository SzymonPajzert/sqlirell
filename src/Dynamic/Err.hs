module Dynamic.Err where

import Control.Monad (liftM)
import Control.Applicative()

data Err b a = Ok a | Bad b
  deriving (Read, Show, Eq, Ord)

instance Monad (Err b) where
  return      = Ok
--fail        = Bad
  Ok a  >>= f = f a
  Bad s >>= _ = Bad s

instance Applicative (Err b) where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o  = liftM f o

instance Functor (Err b) where
  fmap = liftM
