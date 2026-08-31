module Cli (IndexOptions (..), LocateOptions (..), Command (..), parser, cliParser) where

import Options.Applicative

data IndexOptions = IndexOptions
  { indexDatabase :: Maybe FilePath,
    indexFilterPrefix :: Maybe Text,
    indexExtraScopes :: [Text],
    indexVerbose :: Bool
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
  | Version
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

-- Parser for index command
indexParser :: Parser IndexOptions
indexParser =
  IndexOptions
    <$> optional
      ( strOption
          ( long "db"
              <> short 'd'
              <> metavar "PATH"
              <> help "Directory where the index is stored"
          )
      )
    <*> optional
      ( strOption
          ( long "filter-prefix"
              <> metavar "FILTER_PREFIX"
              <> help "Only add paths starting with PREFIX"
          )
      )
    <*> fmap
      (\xs -> if null xs then defaultExtraScopes else xs)
      ( many
          ( strOption
              ( long "extra-scopes"
                  <> metavar "SCOPE"
                  <> help "Extra scopes to index [default: haskellPackages rPackages coqPackages texlive.pkgs ocamlPackages]"
              )
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
      )

-- Parser for locate command
locateParser :: Parser LocateOptions
locateParser =
  LocateOptions
    <$> optional
      ( strOption
          ( long "db"
              <> short 'd'
              <> metavar "PATH"
              <> help "Directory where the index is stored"
          )
      )
    <*> switch
      ( long "regex"
          <> short 'r'
          <> help "Treat PATTERN as regex"
      )
    <*> argument
      str
      ( metavar "PATTERN"
          <> help "Pattern to search for"
      )

-- Parser for subcommands
commandParser :: Parser Command
commandParser =
  subparser (index <> locate) <|> version
  where
    index =
      command
        "index"
        (info (Index <$> indexParser) (progDesc "Build the search index"))
    locate =
      command
        "locate"
        (info (Locate <$> locateParser) (progDesc "Locate packages by pattern"))
    version = flag' Version (long "version" <> short 'V' <> help "Print version")

-- Main parser with global options
parser :: ParserInfo Command
parser =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "nix-sakuin")

cliParser :: IO Command
cliParser = execParser parser