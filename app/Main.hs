#!/usr/bin/env stack
-- stack --resolver lts-10.2 script --ghc-options -i.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/benchmarker/autogen

{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import qualified Data.Text
import qualified Data.Version
import Options.Applicative
  ( Parser,
    auto,
    command,
    flag,
    flag',
    help,
    info,
    long,
    many,
    option,
    progDesc,
    short,
    showDefault,
    strOption,
    subparser,
    value,
    (<$>),
    (<*>),
    (<|>),
  )
import qualified Paths_benchmarker_cli (version)
import Turtle (options, printf, s, (%))

defaultResultBranch = "benchmarks"

defaultResultName = "result"

defaultFolderPath = "run/{run_id}"

data Verbosity = Terse | Normal | Verbose deriving (Show)

instance Read Verbosity where
  readsPrec _ v = (: []) (intoVerbosity $ read v, "")

newtype BaseOptions = BaseOptions {verbosity :: Verbosity} deriving (Show)

data InitOptions = InitOptions
  { base :: BaseOptions,
    folderPath :: String,
    resultBranch :: String
  }
  deriving (Show)

data Command
  = Version
  | Init InitOptions
  deriving (Show)

intoVerbosity :: Int -> Verbosity
intoVerbosity n
  | n < 1 = Terse
  | n == 1 = Normal
  | otherwise = Verbose

cliVersion :: Data.Text.Text
cliVersion = Data.Text.pack (Data.Version.showVersion Paths_benchmarker_cli.version)

parserBaseOptions :: Parser BaseOptions
parserBaseOptions =
  BaseOptions . intoVerbosity . length
    <$> many
      ( flag'
          ()
          ( long "verbosity"
              <> short 'v'
              <> help "Level of verbosity used for the command output"
          )
      )

parserInitOptions :: Parser InitOptions
parserInitOptions =
  InitOptions <$> parserBaseOptions
    <*> strOption
      ( long "folderPath"
          <> short 'd'
          <> help "Folder path to the benchmark result file"
          <> showDefault
          <> value defaultFolderPath
      )
    <*> strOption
      ( long "resultBranch"
          <> short 'b'
          <> help "The repository branch that the results will be stored on"
          <> showDefault
          <> value defaultResultBranch
      )

parserInit :: Parser Command
parserInit = Init <$> parserInitOptions

parser :: Parser Command
parser =
  Version <$ flag' () (long "version" <> help "Version number")
    <|> subparser (command "init" (info parserInit (progDesc "Initialise repository for benchmarking")))

main :: IO ()
main = do
  x <- options "Command line tool for running and publishing benchmarks to a git repository." parser
  print x
