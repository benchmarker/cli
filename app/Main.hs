#!/usr/bin/env stack
-- stack --resolver lts-10.2 script --ghc-options -i.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/benchmarker/autogen

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text
import qualified Data.Version
import Turtle (s, (%), printf, (<$>), (<>),  Alternative((<|>)), options, Parser, Text )
import Options.Applicative (subparser, command, flag', help, info, long, progDesc)
import qualified Paths_benchmarker_cli

data Command
  = Version
  -- | Init InitOptions
  -- | Run RunOptions
  -- | Compare CompareOptions
  -- | Commit CommitOptions
  deriving (Show)

version :: Turtle.Text
version = Data.Text.pack (Data.Version.showVersion Paths_benchmarker_cli.version)

initOptions :: Parser Command
initOptions = error "not implemented"

parser :: Parser Command
parser =
  Main.Version <$ flag' () (long "version" <> help "Version number")
    <|> subparser (command "init" (info initOptions (progDesc "Initialise repository for benchmarking")))

main :: IO ()
main = do
  x <- Turtle.options "Command line tool for running and publishing benchmarks to a git repository." parser
  print x
