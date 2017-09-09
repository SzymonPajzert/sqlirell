{-# LANGUAGE MultiParamTypeClasses #-}
-- TODO remove options_ghc from every file
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Compile.Abstract where

import Prelude hiding (not)
import qualified Grammar.Abs as Abs

newtype Program = Program [Query]

data Query
  = ExpQuery Expression
  | SelQuery SelectStatement

newtype Variable = Variable String

-- TODO add static check of function arg list
data Expression
  = FunCallExpr FunIdent [Expression]
  | IndexAt Expression IndexRepr
  | PrimaryExpr PrimaryExpression
  | VariableRef Variable
  -- TODO check difference between obj.field and obj[index]

data IndexRepr
  = IndexExpr Expression
  | RandomIndex

data FunIdent
  = Exists
  | Exponention
  | Multiplication
  | Division
  | Modulo
  | Addition
  | Substraction
  | Negative -- minus
  | Positive -- TODO check semantic meaning
  | Concatenation
  | IsNull
  | IsMissing
  | IsUnknown
  | IsBetween
  | LogNegation
  | Equal
  | Smaller
  | Like
  | IsIn
  | Coniunction
  | Alternative -- consider removing one

newtype SelectStatement = SelectStatement ()
newtype PrimaryExpression = PrimaryExpression ()

class Transformable a b where
  transform :: a -> b

instance Transformable Abs.VariableRef Variable where
  transform = _

-- TODO make all transformations instances of the class Transformable
instance Transformable Abs.Program Program where
  transform (Abs.Prog queries) = Program $ map transform queries

instance Transformable Abs.Query Query where
  transform (Abs.ExpQuer expression) = ExpQuery newExpression
    where newExpression = transform expression

instance Transformable Abs.Expression Expression where
  transform expr = case expr of
    Abs.OperExpr opExpr -> transform opExpr

instance Transformable Abs.PathExpr Expression where
  transform (Abs.PrimaPathExp primaryExp) = PrimaryExpr $ transform primaryExp
  transform (Abs.FieldPathExp pathExp variable) = IndexAt indexedExpr index
    where indexedExpr = transform pathExp
          index = transform variable
  transform (Abs.INullPathExp pathExp) = IndexAt indexedExpr RandomIndex
    where indexedExpr = transform pathExp
  transform (Abs.IExprPathExp pathExp indexExp) = IndexAt indexedExpr index
    where indexedExpr = transform pathExp
          index = transform indexExp

instance Transformable Abs.OpExpr Expression where
  transform matchedExpr = case matchedExpr of
    Abs.PathOpExpr pathExpr -> transform pathExpr
    Abs.ExistsOpExpr expr-> FunCallExpr Exists $ map transform [expr]
    Abs.ExponeOpExpr expr1 expr2 -> FunCallExpr Exponention $ map transform [expr1, expr2]
    Abs.MultipOpExpr expr1 expr2 -> FunCallExpr Multiplication $ map transform [expr1, expr2]
    Abs.DivisiOpExpr expr1 expr2 -> FunCallExpr Division $ map transform [expr1, expr2]
    Abs.ModuloOpExpr expr1 expr2 -> FunCallExpr Modulo $ map transform [expr1, expr2]
    Abs.AdditiOpExpr expr1 expr2 -> FunCallExpr Division $ map transform [expr1, expr2]
    Abs.SubstrOpExpr expr1 expr2 -> FunCallExpr Division $ map transform [expr1, expr2]
    Abs.NegationExpr expr -> FunCallExpr Negative $ map transform [expr]
    Abs.PositiveExpr expr -> FunCallExpr Positive $ map transform [expr]
    Abs.ConcatOpExpr expr1 expr2 -> FunCallExpr Division $ map transform [expr1, expr2]
    Abs.IsNullOpExpr expr is -> modifyMeaning is $ FunCallExpr IsNull $ map transform [expr]
    Abs.IsMissOpExpr expr is -> modifyMeaning is $ FunCallExpr IsMissing $ map transform [expr]
    Abs.IsUnknOpExpr expr is -> modifyMeaning is $ FunCallExpr IsUnknown $ map transform [expr]
    Abs.IsBtwnOpExpr expr expr1 expr2 -> FunCallExpr IsBetween $ map transform [expr, expr1, expr2]
    Abs.NoAfteOpExpr expr (Abs.BtwnAfterNot expr1 expr2) ->
      not $ FunCallExpr IsBetween $ map transform [expr, expr1, expr2]
    Abs.NoAfteOpExpr expr1 (Abs.LikeAfterNot expr2) ->
      not $ FunCallExpr Like $ map transform [expr1, expr2]
    Abs.NoAfteOpExpr expr1 (Abs.IsInAfterNot expr2) ->
      not $ FunCallExpr IsIn $ map transform [expr1, expr2]
    Abs.IsEquaOpExpr expr1 expr2 ->       FunCallExpr Equal $ map transform [expr1, expr2]
    Abs.NoEquaOpExpr expr1 expr2 -> not $ FunCallExpr Equal $ map transform [expr1, expr2]
    Abs.GraterOpExpr expr1 expr2 ->       FunCallExpr Smaller $ map transform [expr2, expr1]
    Abs.GrOrEqOpExpr expr1 expr2 -> not $ FunCallExpr Smaller $ map transform [expr1, expr2]
    Abs.SmalerOpExpr expr1 expr2 ->       FunCallExpr Smaller $ map transform [expr1, expr2]
    Abs.SmOrEqOpExpr expr1 expr2 -> not $ FunCallExpr Smaller $ map transform [expr2, expr1]
    Abs.IsLikeOpExpr expr1 expr2 -> FunCallExpr Like $ map transform [expr1, expr2]
    Abs.IsIn__OpExpr expr1 expr2 -> FunCallExpr IsIn $ map transform [expr1, expr2]
    Abs.NegatiOpExp expr -> not $ transform expr
    Abs.ConiunOpExp expr1 expr2 -> FunCallExpr Coniunction $ map transform [expr1, expr2]
    Abs.AlternOpExp expr1 expr2 -> FunCallExpr Alternative $ map transform [expr1, expr2]

not :: Expression -> Expression
not expression = FunCallExpr LogNegation [expression]

modifyMeaning :: Abs.Is -> (Expression -> Expression)
modifyMeaning Abs.YIs = id
modifyMeaning Abs.NIs = not

instance Transformable Abs.VariableRef IndexRepr where
  transform = _ -- TODO

instance Transformable Abs.Expression IndexRepr where
  transform = _ -- TODO

instance Transformable Abs.PrimaryExpr PrimaryExpression where
  transform = _ -- TODO


compile :: Abs.Program -> Program
compile = transform
