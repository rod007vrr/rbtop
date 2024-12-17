module SystemState
  ( SystemState (..),
    gatherSystemState,
  )
where

import CPUCollector (ProcessedCPU, getProcessedCPU)
import Control.Applicative
import MemoryCollector (ProcessedMemory, getProcessedMemory)
import ProcessCollector (ProcessList, getProcessList)

-- | Combined system state data
data SystemState = SystemState
  { -- | timestamp
    -- | Memory statistics
    memoryStats :: ProcessedMemory,
    -- | CPU statistics
    cpuStats :: ProcessedCPU,
    -- | List of process information
    processStats :: ProcessList
  }
  deriving (Show, Eq)

gatherSystemState :: IO (Maybe SystemState)
gatherSystemState =
  liftA3 SystemState
    <$> getProcessedMemory
    <*> getProcessedCPU
    <*> getProcessList