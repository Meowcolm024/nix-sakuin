module Main (main) where

import Control.Monad (forM_, unless)
import Criterion.Main (bench, bgroup, defaultMainWith, env, whnfIO)
import Criterion.Main.Options (defaultConfig)
import Criterion.Types (Config (jsonFile, timeLimit))
import Data.ByteString.Lazy qualified as LBS
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (createDirectoryIfMissing, getFileSize)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), IOMode (WriteMode), hPutStrLn, hSetBuffering, stdout, withFile)
import System.IO.Temp (createTempDirectory)
import System.Process.Typed (proc, readProcess, readProcessStdout_)
import Text.Printf (hPrintf, printf)

data Backend = Tsv | FrCode | NixIndex

backends :: [Backend]
backends = [Tsv, FrCode, NixIndex]

backendName :: Backend -> String
backendName Tsv = "tsv"
backendName FrCode = "frcode"
backendName NixIndex = "nix-index"

data QueryBackend = QueryBackend
  { queryName :: String,
    queryDatabase :: Backend,
    queryOptions :: [String]
  }

queryBackends :: [QueryBackend]
queryBackends =
  [ serial Tsv,
    serial FrCode,
    QueryBackend "frcode-pure" FrCode ["--pure"],
    QueryBackend "frcode-parallel" FrCode ["--parallel"],
    QueryBackend "frcode-c" FrCode ["--cheat"],
    serial NixIndex
  ]
  where
    serial backend = QueryBackend (backendName backend) backend []

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  results <- createTempDirectory "/tmp" "nix-sakuin-rpackages-bench."
  defaultMainWith
    defaultConfig {timeLimit = 1.5, jsonFile = Just (results </> "queries.json")}
    [ env (prepare results) $ \_ ->
        bgroup
          "locate"
          [ bgroup "hit" [bench (queryName backend) $ whnfIO (query results backend hit) | backend <- queryBackends],
            bgroup "miss" [bench (queryName backend) $ whnfIO (query results backend miss) | backend <- queryBackends],
            bench "cabal-launcher" $ whnfIO (readProcessStdout_ $ proc "cabal" ["exec", "true", "--"])
          ]
    ]

prepare :: FilePath -> IO ()
prepare results = do
  putStrLn $ "Benchmark results directory " <> results
  nixpkgs <- readProcessStdout_ $ proc "nix-instantiate" ["--find-file", "nixpkgs"]
  LBS.writeFile (results </> "nixpkgs-source") nixpkgs
  writeFile (results </> "scope.nix") scope
  withFile (results </> "index.csv") WriteMode $ \report -> do
    hPutStrLn report "backend,trial,seconds,bytes"
    forM_ [1, 2] $ \trial ->
      forM_ backends $ \backend -> do
        let database = databaseDir results backend trial
        createDirectoryIfMissing True database
        start <- getMonotonicTimeNSec
        (status, output, errors) <- readProcess $ proc "cabal" ("exec" : indexArgs results backend database)
        end <- getMonotonicTimeNSec
        let logFile = database </> "index.log"
        LBS.writeFile logFile (output <> errors)
        unless (status == ExitSuccess) $ fail $ "Indexing failed. See " <> logFile
        bytes <- getFileSize (database </> indexFile backend)
        unless (bytes > 0) $ fail $ "Index is empty. See " <> logFile
        let seconds = fromIntegral (end - start) / 1e9 :: Double
        hPrintf report "%s,%d,%.6f,%d\n" (backendName backend) trial seconds bytes
        printf "%-9s trial %d  %.2f s  %d bytes\n" (backendName backend) trial seconds bytes
  forM_ queryBackends $ \backend -> do
    found <- query results backend hit
    absent <- query results backend miss
    unless (found == "rPackages.jsonlite.out\n" && LBS.null absent) $
      fail $
        "Unexpected query results from " <> queryName backend

scope :: String
scope =
  unlines
    [ "{ system ? builtins.currentSystem, config ? {}, overlays ? [] }:",
      "let pkgs = import <nixpkgs> { inherit system config overlays; }; in",
      "{ rPackages = { inherit (pkgs.rPackages) jsonlite digest rlang; }; }"
    ]

databaseDir :: FilePath -> Backend -> Int -> FilePath
databaseDir results backend trial = results </> backendName backend <> "-" <> show trial

indexFile :: Backend -> FilePath
indexFile NixIndex = "files"
indexFile backend = "database." <> backendName backend <> ".zst"

indexArgs :: FilePath -> Backend -> FilePath -> [String]
indexArgs results backend database =
  backendOptions
    <> ["--db", database, "--nixpkgs", results </> "scope.nix", "--system", "x86_64-linux", "--extra-scopes", "rPackages"]
  where
    backendOptions = case backend of
      NixIndex -> ["nix-index", "--", "--requests", "32", "--compression", "3"]
      _ -> ["nix-sakuin", "--", "index", "--db-format", backendName backend, "--workers", "32", "--verbose", "2"]

hit, miss :: String
hit = "/jsonlite/DESCRIPTION"
miss = "/nix-sakuin-benchmark-absent"

query :: FilePath -> QueryBackend -> String -> IO LBS.ByteString
query results backend pattern = readProcessStdout_ $ proc "cabal" args
  where
    database = queryDatabase backend
    args =
      ["exec"]
        <> backendOptions
        <> queryOptions backend
        <> ["--db", databaseDir results database 2, "--minimal", "--type", "r", "--whole-name", pattern]
    backendOptions = case database of
      NixIndex -> ["nix-locate", "--"]
      _ -> ["nix-sakuin", "--", "locate", "--db-format", backendName database]
