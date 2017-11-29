module Options where

import           Control.Applicative
import 		 Data.Monoid
import           Options.Applicative

data CommandOptions = CommandOptions
  { dataSource      :: FilePath
  , script          :: FilePath
  , runIteractively :: Bool
  }

commandOptionsParser :: Parser CommandOptions
commandOptionsParser = CommandOptions <$> dataSourcePar <*> scriptPar <*> runInteractivelyPar
    where
        dataSourcePar = option str (short 'f' <> metavar "FILE")
        scriptPar = option str (short 's' <> metavar "FILE")
        runInteractivelyPar = switch (short 'i' <> long "interactive")

parseOptions :: IO CommandOptions
parseOptions = execParser $ info (helper <*> commandOptionsParser) commandOptsInfo
    where commandOptsInfo = fullDesc <> progDesc "SQLirell"
