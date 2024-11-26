module MemoryCollector
  ( ProcessedMemory (..),
    ProcessedMemoryList,
  )
where

data ProcessedMemory = ProcessedMemory
  { -- | Unix timestamp when memory was sampled
    timestamp :: Integer,
    -- | Total system memory in bytes
    totalMem :: Integer,
    -- | Used memory in bytes
    usedMem :: Integer,
    -- | Free memory in bytes
    freeMem :: Integer,
    -- | Cached memory in bytes
    cached :: Integer,
    -- | Buffer memory in bytes
    buffers :: Integer
  }
  deriving (Show, Eq)

type ProcessedMemoryList = [ProcessedMemory]
