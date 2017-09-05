{-# OPTIONS_GHC -Wall -fwarn-tabs -fdefer-typed-holes #-}

module Main where

import Prelude hiding (log, all)

import Options.Applicative

import Control.Monad.Writer
import Control.Applicative ()

import Data.List (groupBy)
import Data.List.Split (splitOn)
import Data.Function (on)

import Text.Printf

import System.Console.ANSI
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Compile.Parse (parseComm, Err(..))
import Util ((|>>), (|>))


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

type Range = (Int, Int)
data Result = Result
  { range :: Range
  , maybeError :: [String]
  , fileName :: FileName
  }

testFile :: Bool -> FileName -> IO Result
testFile ignoreSuccess file = do
  fileCont <- readFile file
  let tests = (helpTestFile fileCont) `zip` ([1..] :: [Int])

  pairs <- (flip mapM) tests (\(test, iden) -> do
     let (result, logs) = runWriter test

     if result && ignoreSuccess then return () else do
       let color = if result then Green else Red
       setSGR [SetColor Foreground Vivid color]
       putStrLn $ "[" ++ file ++ "][" ++ (show iden) ++ "]"
       setSGR [Reset]

       mapM_ putStrLn logs

     return (result, logs))

  let (results, logs) = unzip pairs

  let range = (length (filter id results), length results)
  return (Result range (concat logs) file)

files :: FilePath -> [FileName] -> [FilePath]
files path names = do
  name <- names
  return (path ++ "/" ++ name)

noComments :: [String] -> [String]
noComments = filter pred
  where
    pred ('-':'-':_) = False
    pred _           = True

percentage :: Range -> String
percentage (goodN, allN) = "[" ++
  printf "%.2f" ((fromIntegral (100 * goodN) / (fromIntegral allN)) :: Double)
  ++ "%]"


data DataSource
  = Many [FileName]
  | One FileName

data CommandOptions = CommandOptions
  { dataSource :: DataSource
  , ignoreSuccess  :: Bool
  , errorSummary :: Bool
  }


dataSourceParser :: Parser DataSource
dataSourceParser =
  userFilesParser <|>
  manyFilesParser <|>
  (fmap Many $ pure parseableFiles)
  where
    userFilesParser = option (fmap One str) (short 'p' <> metavar "USERFILES")
    manyFilesParser = option (fmap (Many . pure)  str) (short 'f' <> metavar "FILE ... FILE") -- TODO revert

parseOptions :: IO CommandOptions
parseOptions = execParser $ info (helper <*> commandOptsParser) commandOptsInfo
  where
    commandOptsParser = CommandOptions <$> dataSourceParser <*> ignoreSuccessParser <*> errorSummaryParser
    commandOptsInfo = fullDesc <> progDesc "SQLirell tester"
    ignoreSuccessParser = flag False True (short 'i')
    errorSummaryParser = flag False True (short 'e')

generateFormat :: String -> [FormatCommands]
generateFormat ('{':number:'}':rest)  = (Paste $ fromEnum number - 48) : (generateFormat rest)
generateFormat (c : rest) = (Normal c) : (generateFormat rest)
generateFormat [] = []

data FormatCommands
  = Normal Char
  | Paste Int

nth :: Int -> [a] -> a
nth 0 (x:_) = x
nth n (x:xs) = nth (n-1) xs

format :: String -> [String] -> String
format fmt args = do
  f <- generateFormat fmt
  case f of
    Normal c -> return c
    Paste word -> nth word args

main :: IO ()
main = do
  args <- getArgs

  putStrLn $ show args -- TODO remove

  options <- parseOptions


  filesToTest <- case dataSource options of
    Many files -> return files
    One fileOfFiles -> (readFile fileOfFiles) |>> lines |>> noComments

  results <- mapM (testFile (ignoreSuccess options)) filesToTest

  let (good, all) = unzip $ map range results
  let goodNumber = sum good
  let allNumber  = sum all

  putStrLn ""
  putStrLn $ format "{0}/{1} {2} tests passed"
    [show goodNumber, show allNumber, percentage (goodNumber, allNumber)]

  if errorSummary options then do
    let errors = map extract_data results |> concat |> group

    let print_error (error, files) = format "{0} [{1}]" [error, show $ length files]

    mapM_ (putStrLn . print_error) errors
  else return ()

  exitSuccess


-- TODO move to utils
group :: Eq a => [(a, b)] -> [(a, [b])]
group list = do
  grouped <- groupBy ((==) `on` fst) list
  return (fst $ head grouped, map snd grouped)

extract_data (Result _ errors file) = do
  error <- errors
  let split = splitOn "before " error
  guard (length(split) == 2)
  return (last split, file)
