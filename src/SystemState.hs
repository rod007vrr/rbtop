module SystemState
  ( SystemState (..),
  )
where

import CPUCollector (ProcessedCPU)
import MemoryCollector (ProcessedMemory)
import ProcessCollector (Process)

-- | Combined system state data
data SystemState = SystemState
  { -- | Memory statistics
    memoryStats :: ProcessedMemory,
    -- | CPU statistics
    cpuStats :: ProcessedCPU,
    -- | List of process information
    processStats :: [Process]
  }
