{-# LANGUAGE OverloadedStrings #-}
-- TODO ask BNFC on github about {{ and { situation

module Main (main) where

import           Control.Applicative  ()
import           Control.Monad        (when)
import           Control.Monad.Writer
import           Data.Function        (on)
import           Data.List            (groupBy, sortOn)
import           Data.List.Split      (splitOn)
import           Data.Text.Lazy.IO    (putStrLn)
import qualified Text.Format as Form
import           Data.Text.Lazy       (Text, pack, unlines, unpack)
import           Prelude              hiding (all, log, putStrLn, unlines)

import           Options.Applicative
import           System.Console.ANSI
import           System.Exit          (exitSuccess)
import           Text.Printf

import           Compile.Parse        (Err (..), parseComm)
import           Utilities            ((|>), (|>>))


type FileName = String

format str = pack . (Form.format str)

log :: Text -> Writer [Text] ()
log string = tell [string]

helpTestFile :: String -> [Writer [Text] Bool]
helpTestFile fileCont = map interpretResult (parseComm fileCont)
  where
    interpretResult (Ok _) = return True
    interpretResult (Bad err) = do
      log "Compilation error:"
      log $ pack $ show err
      return False

type Range = (Int, Int)
data Result = Result
  { range      :: Range
  , maybeError :: [Text]
  , fileName   :: FileName
  }

testFile :: Bool -> FileName -> IO Result
testFile ignoreSuccess file = do
  fileCont <- readFile file
  let tests = helpTestFile fileCont `zip` ([1..] :: [Int])

  pairs <- mapM (\(test, iden) -> do
     let (result, logs) = runWriter test

     if result && ignoreSuccess then return () else do
       let color = if result then Green else Red
       setSGR [SetColor Foreground Vivid color]
       putStrLn $ format "[{0}]{1}" [file, show iden]
       setSGR [Reset]

       mapM_ putStrLn logs

     return (result, logs)) tests

  let (results, logs) = unzip pairs

  let range = (length (filter id results), length results)
  return (Result range (concat logs) file)

fileNames :: FilePath -> [FileName] -> [FilePath]
fileNames path names = do
  name <- names
  return (path ++ "/" ++ name)

noComments :: [String] -> [String]
noComments = filter pred
  where
    pred ('-':'-':_) = False
    pred _           = True

percentage :: Range -> String
percentage (goodN, allN) = "[" ++
  printf "%.2f" ((fromIntegral (100 * goodN) / fromIntegral allN) :: Double)
  ++ "%]"


data DataSource
  = Many [FileName]
  | One FileName

data CommandOptions = CommandOptions
  { dataSource      :: DataSource
  , doIgnoreSuccess :: Bool
  , doErrorSummary  :: Bool
  }


dataSourceParser :: Parser DataSource
dataSourceParser =
  userFilesParser <|> manyFilesParser
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

main :: IO ()
main = do
  options <- parseOptions

  filesToTest <- case dataSource options of
    Many files      -> return files
    One fileOfFiles -> readFile fileOfFiles 
      |>> lines
      |>> noComments
      |>> map (\file -> "../sqlirrel-tests/parseable/" ++ file)

  results <- mapM (testFile (doIgnoreSuccess options)) filesToTest

  let (good, all) = unzip $ map range results
  let [goodNumber, allNumber] = map sum [good, all]

  putStrLn ""
  putStrLn $ format "{0}/{1} {2} tests passed"
    [show goodNumber, show allNumber, percentage (goodNumber, allNumber)]

  when (doErrorSummary options) $ do
    let errors = map extractData results
          |> concat
          |> group
          |> sortOn (length . snd)

    let print_error (error, files) =
          format "[0]\n[1]" [first_line, rest_lines]
          where first_line = unpack $ format "{0} [{1}]:" [error, show $ length files]
                rest_lines = unpack $ unlines $ map (\file -> format "  {0}" [file]) files

    mapM_ (putStrLn . print_error) errors

  exitSuccess

-- TODO move to utils
group :: Ord a => [(a, b)] -> [(a, [b])]
group list = do
  grouped <- list
    |> sortOn fst
    |> groupBy ((==) `on` fst)
  return (fst $ head grouped, map snd grouped)

extractData :: Result -> [(String, FileName)]
extractData (Result _ errors file) = do
  error <- errors
  let split = splitOn "before " (unpack error)
  guard (length split == 2)
  let failing_string = init $ last split
  return (failing_string, file)
