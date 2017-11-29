module Prompt where

import           Options
import           System.IO       (hFlush, stdout)
import           Utilities       ((|>>))

data PromptCommand
  = Quit
  | Command String
  deriving (Eq)

parseCommand :: String -> PromptCommand
parseCommand "quit"  = Quit
parseCommand command = Command command

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

getPrompt :: String -> IO PromptCommand
getPrompt prompt = readPrompt prompt |>> parseCommand

evalAndPrint :: PromptCommand -> IO CommandOptions
evalAndPrint = _


until_ :: Monad m => b -> (a -> Bool) -> m a -> (a -> m b) -> m b
until_ state predic prompt action = do
  result <- prompt
  if predic result
    then return state
    else do
      newState <- action result
      until_ newState predic prompt action
