module Utils (intoVerbosity) where

import Types

intoVerbosity :: Int -> Verbosity
intoVerbosity n
  | n < 1 = Terse
  | n == 1 = Normal
  | otherwise = Verbose
