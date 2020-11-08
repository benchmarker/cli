{-# LANGUAGE OverloadedStrings #-}

module Parsers (parser) where

import qualified Constants
import Options.Applicative
  ( Parser,
    command,
    flag',
    help,
    info,
    long,
    many,
    progDesc,
    short,
    showDefault,
    strOption,
    subparser,
    value,
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
          <> value Constants.defaultFolderPath
      )
    <*> strOption
      ( long "resultBranch"
          <> short 'b'
          <> help "The repository branch that the results will be stored on"
          <> showDefault
          <> value Constants.defaultResultBranch
      )

parserInit :: Parser Command
parserInit = Init <$> parserInitOptions

parser :: Parser Command
parser =
  Version <$ flag' () (long "version" <> help "Version number")
    <|> subparser (command "init" (info parserInit (progDesc "Initialise repository for benchmarking")))
