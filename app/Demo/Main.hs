module Main where

import Options.Applicative
import Control.Applicative ()
import Data.Monoid
import Compile.Compile (compile) -- TODO rename
import Dynamic.Import (toValue, len)
import Dynamic.Eval (runWith)
import Text.Format (format)
import Dynamic.Syntax (Value(..))

data CommandOptions = CommandOptions
  { dataSource      :: FilePath
  , script          :: FilePath
  }

parseOptions :: IO CommandOptions
parseOptions = execParser $ info (helper <*> commandOptsParser) commandOptsInfo
  where
    commandOptsParser = CommandOptions <$> dataSourceParser <*> scriptParser
    commandOptsInfo = fullDesc <> progDesc "SQLirrel demo"
    dataSourceParser = option str (short 'd' <> metavar "DATA")
    scriptParser = option str (short 's' <> metavar "SCRIPT")

main :: IO ()
main = do
  commands <- parseOptions

  expression <- fmap compile (readFile $ dataSource commands)
  dataM <- toValue <$> (readFile $ script commands)

  case dataM of
    Nothing -> putStrLn "Couldn't parse input data"
    (Just input) -> do
      putStrLn "Calculating"
      let result = expression `runWith` input
      putStrLn (format "Result has {0} lines" (show $ len result))