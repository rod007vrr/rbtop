module SystemState
  ( SystemState (..),
  )
where

import MemoryCollector (ProcessedMemory)
import ProcessCollector (ProcessedProcess)

data SystemState = SystemState
  { -- | Memory statistics
    memoryStats :: ProcessedMemory,
    -- | List of process information
    processStats :: [ProcessedProcess]
  }
  deriving (Show, Eq)
