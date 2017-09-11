module Dynamic.Conversion where

import Dynamic.Syntax
import Dynamic.IdFeeder

toObject :: Expression -> IdFeeder ObjectExpression
toObject expression = do

  x <- getNewIdentifier
  y <- getNewIdentifier
  
  let [xBind, yBind] = map VariableBinding [x, y]
  let objectIter = ObjectIterator x y expression
  let objectCompr = ObjectComprehension xBind yBind $ objectIter
  return $ ObjectExpression $ ComprExpr objectCompr        

toArray :: Expression -> IdFeeder ArrayExpression
toArray expression = do

  x <- getNewIdentifier
  y <- getNewIdentifier
  
  let [_, yBind] = map VariableBinding [x, y]
  let objectIter = ObjectIterator x y expression
  let arrayCompr = ArrayComprehension yBind objectIter 
  return $ ArrayExpression $ ComprExpr arrayCompr

toBag :: Expression -> IdFeeder BagExpression
toBag expression = do
  
  x <- getNewIdentifier
  y <- getNewIdentifier
  
  let [_, yBind] = map VariableBinding [x, y]
  let objectIter = ObjectIterator x y expression
  let bagCompr = BagComprehension yBind objectIter 
  return $ BagExpression $ ComprExpr bagCompr
