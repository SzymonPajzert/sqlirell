module Compile.Parse (parse, parseComm, Err(..)) where

import Prelude hiding (lex)
import Grammar.Par (pProgram, myLexer)
import Grammar.Lex (Token(..), Posn(..), Tok(..))
import qualified Grammar.Abs as Abs
import Grammar.ErrM

import qualified Data.Set as Set

import Data.List.Split (splitWhen)
import Data.Char (toLower)

import Compile.Abstract (compile, Program)
import Utilities ((|>))

-- TODO create better solution
repl :: Char -> Char
repl c = if c == '\'' then '"' else c

lex :: String -> [Token]
lex = myLexer . map (repl . toLower)

parse :: String -> Err Abs.Program
parse = pProgram . lex

commandDelim :: Tok
commandDelim = let [PT _ token] = myLexer ";"in token

isCommandDelim :: Token -> Bool
isCommandDelim (PT _ token) = token == commandDelim
isCommandDelim _            = False

-- TODO create better solution
isAllowed :: [Token] -> Bool
isAllowed = all (not . isBannedWord)
  where
    isBannedWord (PT _ (T_Identifier word)) = Set.member word bannedWords
    isBannedWord _                          = False

    -- TODO reuse tests without ignoring those containing these words
    bannedWords = Set.fromList
      ["use", "drop", "create", "write", "set", "load", "insert", "delete", "connect", "feed", "function", "into", "start"]
      -- TODO support count(*)

lexMany file = lex file |>
  splitWhen isCommandDelim |>
  filter (not . null) |>
  map (\command -> command ++ [PT (Pn 0 0 0) commandDelim]) |>
  filter isAllowed

parseComm :: String -> [Err Program]
parseComm content = content
  |> lexMany |> map pProgram |> map (fmap compile)
