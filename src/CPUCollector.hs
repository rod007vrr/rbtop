{-# LANGUAGE ImportQualifiedPost #-}

module CPUCollector
  ( ProcessedCPU (..),
    ProcessedCPUList,
    getRawCPUData,
    -- processCPUData,
    -- parseCPUStats,
    -- printCPUStats,
    printRawCPUStats,
  )
where

import Control.Applicative
import Parser (Parser (..))
import Parser qualified as P
import System.Process
import Prelude hiding (filter)

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
    idleUsage :: Double
  }
  deriving (Show, Eq)

data RawCPU = RawCPU
  { user :: Double,
    sys :: Double,
    idle :: Double
  }
  deriving (Show, Eq)

type ProcessedCPUList = [ProcessedCPU]

getRawCPUData :: IO String
getRawCPUData = do
  result <- readCreateProcess (shell "top -l 1 -n 0 | grep \"CPU usage\"") ""
  putStrLn $ "Debug - Raw data length: " ++ show (length result)
  putStrLn $ "Debug - Raw data bytes: " ++ show (map (fromEnum) result)
  return result

processRawCPUData :: Parser RawCPU
processRawCPUData =
  RawCPU
    <$> (P.string "CPU usage:" *> P.char ' ' *> P.double <* P.string "% user,")
    <*> (P.char ' ' *> P.double <* P.string "% sys,")
    <*> (P.char ' ' *> P.double <* P.string "% idle" <* many (P.satisfy (\c -> c == ' ' || c == '\n')))

rawToFull :: RawCPU -> ProcessedCPU
rawToFull r =
  ProcessedCPU 0 (user r + sys r) (user r) (sys r) (idle r)

-- | Run and print raw CPU stats
printRawCPUStats :: IO ()
printRawCPUStats = do
  rawData <- getRawCPUData
  putStrLn $ "Raw data: '" ++ rawData ++ "'"
  case P.parse processRawCPUData rawData of
    Right rawCpu -> print $ rawToFull rawCpu
    Left err -> putStrLn err
