{-# LANGUAGE ImportQualifiedPost #-}

module MemoryCollector
  ( ProcessedMemory (..),
    RawMemory (..),
    printRawMemoryStats,
    getProcessedMemory,
    monitorMemory,
    processMemoryData,
    toProcessedMemory,
  )
where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Parser (Parser)
import Parser qualified as P
import System.Process (readProcess)
import Prelude hiding (filter)

data ProcessedMemory = ProcessedMemory
  { -- | Total system memory in bytes
    totalMem :: Int,
    -- | Used memory in bytes
    usedMem :: Int,
    -- | Free memory in bytes
    freeMem :: Int,
    freeMemPercent :: Double
  }
  deriving (Show, Eq)

data RawMemory = RawMemory
  { totalMemSize :: Int, -- in bytes
    totalPages :: Int,
    pageSize :: Int, -- in bytes
    freePages :: Int,
    purgeablePages :: Int,
    pagesPurged :: Int,
    activePages :: Int,
    inactivePages :: Int,
    speculativePages :: Int,
    wiredPages :: Int,
    compressorPages :: Int,
    decompressedPages :: Int,
    compressedPages :: Int,
    pageIn :: Int,
    pageOut :: Int,
    memFreePercent :: Double
  }
  deriving (Show, Eq)

getRawMemoryData :: IO String
getRawMemoryData = readProcess "memory_pressure" [] ""

processMemoryData :: Parser RawMemory
processMemoryData =
  RawMemory
    <$> (P.string "The system has " *> P.int <* P.string " (")
    <*> (P.int <* P.string " pages with a page size of ")
    <*> (P.int <* P.string ").\n\nStats: \nPages free: ")
    <*> (P.int <* P.string " \nPages purgeable: ")
    <*> (P.int <* P.string " \nPages purged: ")
    <*> ( P.int
            <* P.string " \n\nSwap I/O:\nSwapins: "
            *> P.int
            <* P.string " \nSwapouts: "
            *> P.int
            <* P.string " \n\nPage Q counts:\nPages active: "
        )
    <*> (P.int <* P.string " \nPages inactive: ")
    <*> (P.int <* P.string " \nPages speculative: ")
    <*> (P.int <* P.string " \nPages throttled: " *> P.int <* P.string " \nPages wired down: ")
    <*> (P.int <* P.string " \n\nCompressor Stats:\nPages used by compressor: ")
    <*> (P.int <* P.string " \nPages decompressed: ")
    <*> (P.int <* P.string " \nPages compressed: ")
    <*> (P.int <* P.string " \n\nFile I/O:\nPageins: ")
    <*> (P.int <* P.string " \nPageouts: ")
    <*> (P.int <* P.string " \n\nSystem-wide memory free percentage: ")
    <*> (P.double <* P.char '%' <* many (P.satisfy (\c -> c == ' ' || c == '\n')))

-- | Run and print raw memory stats
printRawMemoryStats :: IO ()
printRawMemoryStats = do
  rawData <- getRawMemoryData
  case P.parse processMemoryData rawData of
    Right rawMemory -> print rawMemory
    Left err -> putStrLn err

-- | Convert RawMemory to ProcessedMemory
toProcessedMemory :: RawMemory -> ProcessedMemory
toProcessedMemory raw =
  ProcessedMemory
    { totalMem = totalMemSize raw,
      freeMem = freePages raw * pageSize raw,
      usedMem = totalMemSize raw - (freePages raw * pageSize raw),
      freeMemPercent = memFreePercent raw
    }

-- | Get processed memory statistics
getProcessedMemory :: IO (Maybe ProcessedMemory)
getProcessedMemory = do
  rawData <- getRawMemoryData
  case P.parse processMemoryData rawData of
    Right rawMemory -> do
      return $ Just $ toProcessedMemory rawMemory
    Left _ -> return Nothing

-- | Monitor memory usage by polling once per second
monitorMemory :: IO ()
monitorMemory = forever $ do
  maybeMemory <- getProcessedMemory
  case maybeMemory of
    Just mem -> do
      print "FINAL PROCESSED!!!!!!!!!!!!"
      print mem
    Nothing -> putStrLn "Failed to get memory stats"
  threadDelay 1000000 -- 1 second delay
