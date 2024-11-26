module ProcessCollector
  ( ProcessedProcess (..),
    ProcessedProcessList,
  )
where

data ProcessedProcess = ProcessedProcess
  { -- | Unix timestamp when process was sampled
    timestamp :: Integer,
    -- | Process ID
    pid :: Integer,
    -- | Process name/command
    name :: String,
    -- | CPU usage percentage (0-100)
    cpuPercent :: Double,
    -- | Memory usage in bytes
    memoryUsage :: Integer,
    -- | Virtual memory size in bytes
    virtualMemory :: Integer,
    -- | Resident set size in bytes
    residentMemory :: Integer
  }
  deriving (Show, Eq)

type ProcessedProcessList = [ProcessedProcess]
