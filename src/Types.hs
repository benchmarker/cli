module Types where

data Verbosity = Terse | Normal | Verbose deriving (Show)

-- Parse string as verbosity type
-- instance Read Verbosity where
--   readsPrec _ v = (: []) (intoVerbosity $ read v, "")

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
