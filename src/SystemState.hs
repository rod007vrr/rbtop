module SystemState
  ( SystemState (..),
  )
where

import CPUCollector (ProcessedCPU)
import MemoryCollector (ProcessedMemory)
import ProcessCollector (ProcessList)

-- | Combined system state data
data SystemState = SystemState
  { -- | timestamp
    time :: Integer,
    -- | Memory statistics
    memoryStats :: ProcessedMemory,
    -- | CPU statistics
    cpuStats :: ProcessedCPU,
    -- | List of process information
    processStats :: ProcessList
  }
