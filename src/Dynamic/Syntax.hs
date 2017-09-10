module Dynamic.Syntax where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

data Value
  = AtomicValue Atomic
  | ObjectValue Object
  | BagValue Bag
  | ArrayValue Array

data Atomic
  = Concrete AtomicConcrete
  | Null
  | Missing

data AtomicConcrete
  = AtomicString String
  | AtomicNumber Int
  | AtomicBool Bool
  
type ObjectKey = String
type Object = Map ObjectKey Value
type Bag = Set Value
type Array = [Value]

type Identifier = String

data Expression
  = ValueExpr Value
  | VariableBinding Identifier
  | ComprExpr Comprehension

data Iterator
  = ArrayIterator Identifier Expression
  | ObjectIterator Identifier Identifier Expression
  | SequenceIterator Iterator Iterator
  | EmptyIterator
  | IderatorModifier IteratorModification Iterator

data IteratorModification
  = Sorting [Identifier]
  | Grouping [Identifier]
  | Numbering Identifier

data Comprehension
  = ObjectComprehension Expression Expression Iterator
  | ArrayComprehension Expression Iterator
  | BagComprehension Expression Iterator


toObject :: Expression -> Object
toArray :: Expression -> Array
toBag :: Expression -> Bag
