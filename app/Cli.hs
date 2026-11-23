module Cli where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Version (showVersion)
import Options.Applicative
import Path
import Paths_nix_sakuin (version)
import Sakuin.Database (DatabaseFormat (..), defaultDatabaseFormat)

data IndexOptions = IndexOptions
  { indexDatabase :: Maybe (Path Abs Dir),
    indexDatabaseFormat :: DatabaseFormat,
    indexFilterPrefix :: Maybe Text,
    indexSystem :: Maybe Text,
    indexWorker :: Int,
    indexNixpkgsPath :: Text,
    indexExtraScopes :: [Text],
    indexNoDefaultScope :: Bool,
    indexFetchCache :: Bool,
    indexVerbose :: Int
  }
  deriving stock (Show)

data LocateOptions = LocateOptions
  { locateDatabase :: Maybe (Path Abs Dir),
    locateDatabaseFormat :: DatabaseFormat,
    locateParallel :: Bool,
    locateRipgrep :: Bool,
    locateCheat :: Bool,
    locateRegex :: Bool,
    locatePackage :: Maybe Text,
    locateHash :: Maybe Text,
    locateTypes :: [Char],
    locateWholeName :: Bool,
    locateAtRoot :: Bool,
    locateMinimal :: Bool,
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
    "rocqPackages",
    "texlive.pkgs",
    "ocamlPackages"
  ]

optionMaybe :: ReadM a -> (Mod OptionFields a) -> Parser (Maybe a)
optionMaybe r m = optional (option r m)

databaseOption :: Parser (Maybe (Path Abs Dir))
databaseOption =
  optionMaybe
    ( eitherReader $ \s ->
        case parseAbsDir s of
          Nothing -> Left "Invalid absolute path"
          Just path -> Right path
    )
    ( long "db"
        <> short 'd'
        <> metavar "PATH"
        <> help "Directory where the index is stored (default: $XDG_CACHE_HOME/nix-sakuin)"
    )

databaseFormatOption :: Parser DatabaseFormat
databaseFormatOption =
  option
    (eitherReader readFormat)
    ( long "db-format"
        <> metavar "FORMAT"
        <> value defaultDatabaseFormat
        <> showDefaultWith formatName
        <> help "Database format (frcode or tsv)"
    )
  where
    formatName FrCode = "frcode"
    formatName Tsv = "tsv"
    readFormat "tsv" = Right Tsv
    readFormat "frcode" = Right FrCode
    readFormat _ = Left "FORMAT must be frcode or tsv"

-- Parser for index command
indexParser :: Parser IndexOptions
indexParser = do
  indexDatabase <- databaseOption
  indexDatabaseFormat <- databaseFormatOption
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
    option
      (auto @Int)
      ( long "workers"
          <> short 'w'
          <> metavar "WORKERS"
          <> value 100
          <> showDefault
          <> help "Number of parallel workers"
      )
  indexNixpkgsPath <-
    option
      str
      ( long "nixpkgs"
          <> short 'f'
          <> metavar "NIXPKGS"
          <> value "<nixpkgs>"
          <> showDefault
          <> help "Path to nixpkgs repository"
      )
  indexExtraScopes <-
    ( many $
        strOption $
          long "extra-scopes"
            <> metavar "EXTRA_SCOPES"
            <> help "Extra scopes to index (default: haskellPackages rPackages coqPackages texlive.pkgs ocamlPackages)"
    )
      <&> (\xs -> if null xs then defaultExtraScopes else xs)
  indexNoDefaultScope <- switch (long "no-default-scope" <> help "Do not index default scope")
  indexFetchCache <-
    switch
      ( long "fetch-cache"
          <> help "Cache fetched narinfo and listings in $TMPDIR (or /tmp)"
      )
  indexVerbose <-
    option
      (auto @Int)
      ( long "verbose"
          <> metavar "LEVEL"
          <> value 1
          <> showDefault
          <> help "Verbosity level (0-2)"
      )
  pure $
    IndexOptions
      { indexDatabase,
        indexDatabaseFormat,
        indexFilterPrefix,
        indexSystem,
        indexWorker,
        indexNixpkgsPath,
        indexExtraScopes,
        indexNoDefaultScope,
        indexFetchCache,
        indexVerbose
      }

-- Parser for locate command
locateParser :: Parser LocateOptions
locateParser = do
  locateDatabase <- databaseOption
  locateDatabaseFormat <- databaseFormatOption
  locateParallel <- switch (long "parallel" <> help "Decode FrCode chunks in parallel")
  locateRipgrep <- switch (long "ripgrep" <> help "Prefilter decoded FrCode rows with ripgrep")
  locateCheat <- switch (long "cheat" <> help "Search FrCode using the experimental C backend")
  locateRegex <-
    switch (long "regex" <> short 'r' <> help "Treat PATTERN as regex")
  locatePackage <-
    optionMaybe str (long "package" <> short 'p' <> metavar "PACKAGE" <> help "Only print matches from packages whose name starts with PACKAGE")
  locateHash <-
    optionMaybe str (long "hash" <> metavar "HASH" <> help "Only print matches from the package with HASH")
  locateTypes <-
    many $
      option
        (eitherReader readFileType)
        (long "type" <> short 't' <> metavar "TYPE" <> help "Only print matches of TYPE (r, x, d, or s)")
  locateWholeName <-
    switch (long "whole-name" <> short 'w' <> help "Match only complete paths or path suffixes")
  locateAtRoot <-
    switch (long "at-root" <> help "Match PATTERN starting at the root of a package")
  locateMinimal <-
    switch (long "minimal" <> help "Only print attribute names of found files or directories")
  locatePattern <-
    strArgument (metavar "PATTERN" <> help "Pattern to search for")
  pure $
    LocateOptions
      { locateDatabase,
        locateDatabaseFormat,
        locateParallel,
        locateRipgrep,
        locateCheat,
        locateRegex,
        locatePackage,
        locateHash,
        locateTypes,
        locateWholeName,
        locateAtRoot,
        locateMinimal,
        locatePattern
      }
  where
    readFileType [fileType]
      | fileType `elem` ("rxds" :: String) = Right fileType
    readFileType _ = Left "TYPE must be one of: r, x, d, s"

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
