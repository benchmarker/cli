{-# LANGUAGE OverloadedStrings #-}

module Parsers (parser) where

import qualified Constants
import Options.Applicative
  ( Parser,
    command,
    flag',
    help,
    helper,
    info,
    long,
    many,
    metavar,
    progDesc,
    short,
    showDefault,
    strOption,
    subparser,
    value,
    (<**>),
    (<|>),
  )
import Types
import Utils (intoVerbosity)

parserBaseOptions :: Parser BaseOptions
parserBaseOptions =
  BaseOptions . intoVerbosity . length
    <$> many
      ( flag'
          ()
          ( long "verbosity"
              <> short 'v'
              <> help "Level of verbosity used for the command output e.g. -vv"
          )
      )

parserInitOptions :: Parser InitOptions
parserInitOptions =
  InitOptions <$> parserBaseOptions
    <*> strOption
      ( long "folderPath"
          <> short 'd'
          <> metavar "FOLDER_PATH"
          <> help "Folder path to the benchmark result file"
          <> value Constants.defaultFolderPath
          <> showDefault
      )
    <*> strOption
      ( long "resultBranch"
          <> short 'b'
          <> metavar "REPO_BRANCH"
          <> help "The repository branch that the results will be stored on"
          <> value Constants.defaultResultBranch
          <> showDefault
      )

parserInit :: Parser Command
parserInit = Init <$> parserInitOptions

parser :: Parser Command
parser =
  Version <$ flag' () (long "version" <> help "Version number")
    <|> subparser (command "init" (info (parserInit <**> helper) (progDesc "Initialise repository for benchmarking")))
