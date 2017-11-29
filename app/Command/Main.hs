module Main (main) where

import           Options
import           Prompt
import           Data.Monoid


main :: IO ()
main = do
  args <- parseOptions

  until_ args (== Quit) (getPrompt "sqlirrel>> ") evalAndPrint
  return ()
