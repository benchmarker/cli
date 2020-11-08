module Constants where

import qualified Data.Text
import qualified Data.Version
import qualified Paths_benchmarker_cli (version)

cliVersion :: Data.Text.Text
cliVersion = Data.Text.pack (Data.Version.showVersion Paths_benchmarker_cli.version)

defaultResultBranch = "benchmarks"

defaultResultName = "result"

defaultFolderPath = "run/{run_id}"
