{-# OPTIONS_GHC -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import Prelude hiding (log, all)
import System.Console.ANSI
import Control.Monad.Writer
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Text.Printf

import Compile.Parse (parseComm, Err(..))

import Util ((|>), (|>>))

type FileName = String

parseableFiles :: [FileName]
parseableFiles = "../example/parseable" `files` ["arithmetic.sql", "collection_op.sql"]

log :: String -> Writer [String] ()
log string = tell [string]

helpTestFile :: String -> [Writer [String] Bool]
helpTestFile fileCont =
  (flip map) (parseComm fileCont) (\parsed -> case parsed of
     Bad err -> do
       log "Compilation error:"
       log $ show err
       return False
     Ok _ -> return True)

type Result = (Int, Int)

testFile :: String -> IO Result
testFile file = do
  fileCont <- readFile file
  let tests = (helpTestFile fileCont) `zip` ([1..] :: [Int])

  results <- (flip mapM) tests (\(test, iden) -> do
     let (result, logs) = runWriter test

     let color = if result then Green else Red
     setSGR [SetColor Foreground Vivid color]
     putStrLn $ "[" ++ file ++ "][" ++ (show iden) ++ "]"
     setSGR [Reset]

     mapM_ putStrLn logs

     return result)

  return (length (filter id results), length results)

files :: FilePath -> [FileName] -> [FilePath]
files path names = do
  name <- names
  return (path ++ "/" ++ name)

noComments :: [String] -> [String]
noComments = filter pred
  where
    pred ('-':'-':_) = False
    pred _           = True

percentage :: Result -> String
percentage (goodN, allN) = "[" ++
  printf "%.2f" ((fromIntegral (100 * goodN) / (fromIntegral allN)) :: Double)
  ++ "%]"
main :: IO ()
main = do
  args <- getArgs

  putStrLn $ show args -- TODO remove

  filesToTestMaybe <- case args of
        [] -> return $ Just parseableFiles
        ("-p":userFiles) -> return $ Just userFiles

        ["-f", fileOfFiles] -> (readFile fileOfFiles) |>> lines |>> noComments |>> Just
        [help] | help `elem` ["-h", "--help"] -> return Nothing
        _                                     -> return Nothing

  case filesToTestMaybe of
    Nothing -> usage
    Just filesToTest -> do
        results <- mapM testFile filesToTest

        let (good, all) = unzip results
        let goodNumber = sum good
        let allNumber  = sum all

        putStrLn ""
        putStrLn $ (show goodNumber) ++ "/" ++ (show allNumber) ++ " " ++ (percentage (goodNumber, allNumber)) ++ " tests passed"
        exitSuccess


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  -h --help          Display this help message."
    , "  (no arguments)     Parse content of default files"
    , "  -p (files)         Parse content of user given files"
    ]
  exitFailure
