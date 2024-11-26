{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module ProcessCollector
  ( ProcessedProcess (..),
    ProcessedProcessList,
  )
where

import Data.Function
import Data.List
import Test.QuickCheck

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

-- | Properties for testing sort functions
prop_sortByCPUDesc :: [ProcessedProcess] -> Property
prop_sortByCPUDesc ps =
  not (null ps)
    ==> let sorted = sortByCPU ps
         in all (\(a, b) -> cpuPercent a >= cpuPercent b) $ zip sorted (tail sorted)

prop_sortByMemoryDesc :: [ProcessedProcess] -> Property
prop_sortByMemoryDesc ps =
  not (null ps)
    ==> let sorted = sortByMemory ps
         in all (\(a, b) -> memoryUsage a >= memoryUsage b) $ zip sorted (tail sorted)

prop_sortByPIDAsc :: [ProcessedProcess] -> Property
prop_sortByPIDAsc ps =
  not (null ps)
    ==> let sorted = sortByPID ps
         in all (\(a, b) -> pid a <= pid b) $ zip sorted (tail sorted)

prop_sortByNameAsc :: [ProcessedProcess] -> Property
prop_sortByNameAsc ps =
  not (null ps)
    ==> let sorted = sortByName ps
         in all (\(a, b) -> name a <= name b) $ zip sorted (tail sorted)
