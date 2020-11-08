#!/usr/bin/env stack
-- stack --resolver lts-16.20 script --package optparse-applicative,text,turtle --ghc-options -isrc:.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/benchmarker/autogen

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsers (parser)
import Turtle (options)

main :: IO ()
main = do
  commandOptions <- options "Command line tool for running and publishing benchmarks to a git repository." parser
  -- TODO: Load config from file
  -- TODO: Overwrite config with command options
  -- TODO: Run command with overwritten config
  print commandOptions
