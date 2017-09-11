module Dynamic.IdFeeder where

import Dynamic.Syntax
import Control.Monad.State

type UnusedNames = [Identifier]

cross_join :: [a] -> [[a]] -> [[a]]
cross_join heads tails = do
  t <- tails
  h <- heads
  return $ h : t

allNames :: [Identifier]
allNames = letters `cross_join` allNames
  where letters = ['a'..'z']

type IdFeeder a = State UnusedNames a

getNewIdentifier :: IdFeeder Identifier
getNewIdentifier = do
  (newName : newUnused) <- get
  put newUnused
  return newName
