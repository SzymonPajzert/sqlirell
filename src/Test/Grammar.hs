{-# OPTIONS_GHC -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import Prelude hiding (log)
import System.Console.ANSI
import Control.Monad.Writer

import Grammar.Parse (parse, Err(..))

type FileName = String

parseableFiles :: [FileName]
parseableFiles = []

log :: String -> Writer [String] ()
log string = tell [string]

helpTestFile :: String -> Writer [String] Bool
helpTestFile fileCont =
  case parse fileCont of
     Bad err -> do
       log "Compilation error:"
       log $ show err
       return False
     Ok _ -> do
       log "Ok"
       return True
       
testFile :: String -> IO Bool
testFile file = do
  fileCont <- readFile file
  let (result, logs) = runWriter $ helpTestFile fileCont

  let color = if result then Green else Red
  setSGR [SetColor Foreground Vivid color]
  putStrLn $ "[" ++ file ++ "]"
  setSGR [Reset]

  mapM_ putStrLn logs

  return result

files :: FilePath -> [FileName] -> [FilePath]
files path names = do
  name <- names
  return (path ++ "/" ++ name)

main :: IO ()
main = do
  results <- mapM testFile parseableFiles
  let number = length $ filter not results

  putStrLn ""
  putStrLn $ (show number) ++ " tests failed"

