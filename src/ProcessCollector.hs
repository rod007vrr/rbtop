{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module ProcessCollector
  ( ProcessedProcess (..),
    ProcessedProcessList,
    sortByCPU,
    sortByMemory,
    sortByPID,
    sortByName,
  )
where

import Data.Function
import Data.List

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

-- | Sort processes by CPU usage (descending)
sortByCPU :: [ProcessedProcess] -> [ProcessedProcess]
sortByCPU = sortBy (flip compare `on` cpuPercent)

-- | Sort processes by memory usage (descending)
sortByMemory :: [ProcessedProcess] -> [ProcessedProcess]
sortByMemory = sortBy (flip compare `on` memoryUsage)

-- | Sort processes by PID (ascending)
sortByPID :: [ProcessedProcess] -> [ProcessedProcess]
sortByPID = sortBy (compare `on` pid)

-- | Sort processes by name (ascending)
sortByName :: [ProcessedProcess] -> [ProcessedProcess]
sortByName = sortBy (compare `on` name)
