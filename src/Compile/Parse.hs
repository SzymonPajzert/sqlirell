module Compile.Parse (parse, parseComm, Err(..)) where

import Prelude hiding (lex)
import Grammar.Par (pProgram, myLexer)
import Grammar.Lex (Token(..), Posn(..), Tok(..))
import Grammar.ErrM

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List.Split (splitWhen)
import Data.Char (toLower)

import Util ((|>))

-- TODO create better solution
repl :: Char -> Char
repl c = if c == '\'' then '"' else c

lex = myLexer . (map repl) . (map toLower)

parse = pProgram . lex

commandDelim =
  let [(PT _ token)] = myLexer ";"
  in token

isCommandDelim (PT _ token) = (token == commandDelim)
isCommandDelim _            = False

-- TODO create better solution
isAllowed :: [Token] -> Bool
isAllowed = all (not . isBannedWord)
  where
    isBannedWord (PT _ (T_Identifier word)) = Set.member word bannedWords
    isBannedWord _                          = False

    bannedWords = Set.fromList
      ["use", "drop", "create", "write", "set", "load", "insert"]

lexMany file = (lex file) |>
  splitWhen isCommandDelim |>
  filter (not . null) |>
  map (\command -> command ++ [(PT (Pn 0 0 0) commandDelim)]) |>
  filter isAllowed

parseComm = (map pProgram) . lexMany
