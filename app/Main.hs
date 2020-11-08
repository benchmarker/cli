#!/usr/bin/env stack
-- stack --resolver lts-10.2 script --ghc-options -isrc:.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/benchmarker/autogen

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsers (parser)
import Turtle (options)

main :: IO ()
main = do
  x <- options "Command line tool for running and publishing benchmarks to a git repository." parser
  print x
