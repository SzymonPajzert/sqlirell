module Grammar.Parse (parse, Err(..)) where

import Grammar.Par (pProgram, myLexer) 
import Grammar.ErrM 
import Debug.Trace (trace)

-- TODO create better solution
repl :: Char -> Char
repl c = if c == '\'' then '"' else c

parse = pProgram . myLexer . (map repl)
