module Grammar.Parse (parse, Err(..)) where

import Grammar.Par (pExpression, myLexer) 
import Grammar.ErrM 

parse = pExpression . myLexer
