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
  { memoryStats :: ProcessedMemory,
    cpuStats :: ProcessedCPU,
    processStats :: ProcessList
  }
  deriving (Show, Eq)

gatherSystemState :: IO (Maybe SystemState)
gatherSystemState =
  liftA3 SystemState
    <$> getProcessedMemory
    <*> getProcessedCPU
    <*> getProcessList