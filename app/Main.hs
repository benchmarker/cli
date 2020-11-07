#!/usr/bin/env stack exec ghci -- -i.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/benchmarker/autogen
-- stack --resolver lts-10.2 script

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text
import qualified Data.Version
import Options.Applicative (Parser, flag', help, long)
import qualified Paths_benchmarker_cli (version)
import qualified Turtle

data Command
  = Version
  --   | Init InitOptions
  --   | Run RunOptions
  --   | Compare CompareOptions
  --   | Commit CommitOptions
  deriving (Show)

parser :: Parser Command
parser = Main.Version <$ flag' () (long "version" <> help "Version number")

-- <> subparser (command "init" (info addOptions ( progDesc "Initialise repository for benchmarking" )))

version :: Turtle.Text
version = Data.Text.pack (Data.Version.showVersion Paths_benchmarker_cli.version)

main :: IO ()
main = do
  x <- Turtle.options "Command line tool for running and publishing benchmarks to a git repository." parser
  Turtle.printf Turtle.s version
