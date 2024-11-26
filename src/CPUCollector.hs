module CPUCollector
  ( ProcessedCPU (..),
    ProcessedCPUList,
  )
where

data ProcessedCPU = ProcessedCPU
  { -- | Unix timestamp when CPU was sampled
    timestamp :: Integer,
    -- | Total CPU usage percentage (0-100)
    totalUsage :: Double,
    -- | User space CPU usage percentage (0-100)
    userUsage :: Double,
    -- | System/kernel CPU usage percentage (0-100)
    systemUsage :: Double,
    -- | Idle CPU percentage (0-100)
    idleUsage :: Double,
    -- | IO wait percentage (0-100)
    ioWaitUsage :: Double
  }
  deriving (Show, Eq)

type ProcessedCPUList = [ProcessedCPU]
