module Cli where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Version (showVersion)
import Options.Applicative
import Paths_nix_sakuin (version)

data IndexOptions = IndexOptions
  { indexDatabase :: Maybe Text,
    indexFilterPrefix :: Maybe Text,
    indexSystem :: Maybe Text,
    indexWorker :: Int,
    indexExtraScopes :: [Text],
    indexNoDefaultScope :: Bool,
    indexVerbose :: Int
  }
  deriving stock (Show)

data LocateOptions = LocateOptions
  { locateDatabase :: Maybe FilePath,
    locateRegex :: Bool,
    locatePattern :: Text
  }
  deriving stock (Show)

data Command
  = Index IndexOptions
  | Locate LocateOptions
  deriving stock (Show)

-- Default extra scopes
defaultExtraScopes :: [Text]
defaultExtraScopes =
  [ "haskellPackages",
    "rPackages",
    "coqPackages",
    "texlive.pkgs",
    "ocamlPackages"
  ]

optionMaybe :: (Read a) => ReadM a -> (Mod OptionFields a) -> Parser (Maybe a)
optionMaybe r m = optional (option r m)

-- Parser for index command
indexParser :: Parser IndexOptions
indexParser = do
  indexDatabase <-
    optionMaybe
      str
      ( long "db"
          <> short 'd'
          <> metavar "PATH"
          <> help "Directory where the index is stored"
      )
  indexFilterPrefix <-
    optionMaybe
      str
      ( long "filter-prefix"
          <> metavar "FILTER_PREFIX"
          <> help "Only add paths starting with PREFIX"
      )
  indexSystem <-
    optionMaybe
      str
      ( long "system"
          <> short 's'
          <> metavar "PLATFORM"
          <> help "Specify system platform for which to build the index"
      )
  indexWorker <-
    optionMaybe
      (auto @Int)
      ( long "workers"
          <> short 'w'
          <> metavar "WORKERS"
          <> value 100
          <> showDefault
          <> help "Number of parallel workers"
      )
      <&> fromMaybe 100
  indexExtraScopes <-
    ( many $
        strOption $
          long "extra-scopes"
            <> metavar "EXTRA_SCOPES"
            <> help "Extra scopes to index (default: haskellPackages rPackages coqPackages texlive.pkgs ocamlPackages)"
    )
      <&> (\xs -> if null xs then defaultExtraScopes else xs)
  indexNoDefaultScope <- switch (long "no-default-scope" <> help "Do not index default scope")
  indexVerbose <-
    optionMaybe
      (auto @Int)
      ( long "verbose"
          <> metavar "LEVEL"
          <> value 1
          <> showDefault
          <> help "Verbosity level (0-2)"
      )
      <&> fromMaybe 1
  pure $
    IndexOptions
      { indexDatabase,
        indexFilterPrefix,
        indexSystem,
        indexWorker,
        indexExtraScopes,
        indexNoDefaultScope,
        indexVerbose
      }

-- Parser for locate command
locateParser :: Parser LocateOptions
locateParser = do
  locateDatabase <-
    optionMaybe
      str
      ( long "db"
          <> short 'd'
          <> metavar "PATH"
          <> help "Directory where the index is stored"
      )
  locateRegex <-
    switch (long "regex" <> short 'r' <> help "Treat PATTERN as regex")
  locatePattern <-
    strArgument (metavar "PATTERN" <> help "Pattern to search for")
  pure $ LocateOptions locateDatabase locateRegex locatePattern

-- Parser for subcommands
commandParser :: Parser Command
commandParser = subparser (index <> locate)
  where
    index = command "index" (info (Index <$> indexParser) (progDesc "Build the search index"))
    locate = command "locate" (info (Locate <$> locateParser) (progDesc "Locate packages by pattern"))

-- Main parser with global options
parser :: ParserInfo Command
parser =
  info
    (commandParser <**> simpleVersioner ("nix-sakuin " <> showVersion version) <**> helper)
    (fullDesc <> progDesc "nix-sakuin")

cliParser :: IO Command
cliParser = execParser parser
