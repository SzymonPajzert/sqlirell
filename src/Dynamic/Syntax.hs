module Dynamic.Syntax where

import Data.Map (Map)
import Data.Set (Set)

data Value
  = ObjectValue Object
  | BagValue Bag
  | ArrayValue Array
  | Null
  | Missing
  | AtomicString String
  | AtomicNumber Int
  | AtomicBool Bool
  deriving (Show, Ord, Eq)

-- ask whether to evaluate 
type ObjectKey = String
type Object = Map ObjectKey Value
type Bag = Set Value
type Array = [Value]

type Identifier = String

data Expression
  = ValueExpr Value
  | VariableBinding Identifier
  | ComprExpr Comprehension
  deriving (Show)

data Iterator
  = ArrayIterator Identifier Expression
  | ObjectIterator Identifier Identifier Expression
  | SequenceIterator Iterator Iterator
  | EmptyIterator
  | IteratorModifier IteratorModification Iterator

data IteratorModification
  = Sorting [Identifier]
  | Grouping [Identifier]
  | Numbering Identifier

instance Show Comprehension where
  show _ = "Comprehension"

data Comprehension
  = ObjectComprehension Expression Expression Iterator
  | ArrayComprehension Expression Iterator
  | BagComprehension Expression Iterator


data Operation
  = ObjectUnion ObjectExpression ObjectExpression
  | FieldSelection ObjectExpression Expression
  | ArrayConcat ArrayExpression ArrayExpression
  | ArrayIndexing ArrayExpression Expression
  | BagUnion BagExpression BagExpression
  | EmptyBagTest BagExpression BoolExpression

-- we denote in a type system that values have to be converted
newtype ObjectExpression = ObjectExpression Expression
newtype ArrayExpression = ArrayExpression Expression
newtype BagExpression = BagExpression Expression
newtype BoolExpression = BoolExpression Expression
