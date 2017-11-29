module CompileTest where

import qualified Compile.Abstract    as Com
import Grammar.Abs
import           Test.HUnit

programs :: [Abs.Program]
programs = [basic] where
  basic = Abs.Prog [value, query]
  value = 