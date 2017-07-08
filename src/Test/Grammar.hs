{-# OPTIONS_GHC -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import Prelude hiding (log)
import System.Console.ANSI
import Control.Monad.Writer
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Compile.Parse (parse, Err(..))

type FileName = String

parseableFiles :: [FileName]
parseableFiles = "../example/parseable" `files` ["arithmetic.sql", "collection_op.sql"]

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

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  -h --help          Display this help message."
    , "  (no arguments)     Parse content of default files"
    , "  -p (files)         Parse content of user given files"
    ]
  exitFailure

-- TODO move to util
infixr 5 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) a f = f `fmap` a

infixr 5 |>
(|>) :: a -> (a -> b) -> b
(|>) a f = f a

noComments :: [String] -> [String]
noComments = filter pred
  where
    pred ('-':'-':_) = False
    pred _           = True

main :: IO ()
main = do
  args <- getArgs

  putStrLn $ show args -- TODO remove

  filesToTestMaybe <- case args of
        [] -> return $ Just parseableFiles
        ("-p":userFiles) -> return $ Just userFiles

        -- TODO pipe like ocaml infix operator on monads
        
        ["-f", fileOfFiles] -> (readFile fileOfFiles) |>> lines |>> noComments |>> Just
        [help] | help `elem` ["-h", "--help"] -> return Nothing
        _                                     -> return Nothing

  case filesToTestMaybe of
    Nothing -> usage
    Just filesToTest -> do
        results <- mapM testFile filesToTest
        let number = length $ filter id results
        let allNumber = length $ results

        putStrLn ""
        putStrLn $ (show number) ++ "/" ++ (show allNumber) ++ " tests passed"
        exitSuccess

