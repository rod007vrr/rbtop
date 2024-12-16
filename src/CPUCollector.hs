{-# LANGUAGE ImportQualifiedPost #-}

module CPUCollector
  ( ProcessedCPU (..),
    ProcessedCPUList,
    getRawCPUData,
    printRawCPUStats,
  )
where

import Control.Applicative
import Parser (Parser)
import Parser qualified as P
import System.Process
import Prelude hiding (filter)

data ProcessedCPU = ProcessedCPU
  { -- | CPU usage percentage
    totalUsage :: Double,
    -- | User space CPU usage percentage
    userUsage :: Double,
    -- | System/kernel CPU usage percentage
    systemUsage :: Double,
    -- | Idle CPU percentage
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
getRawCPUData =
  readCreateProcess (shell "top -l 1 -n 0 | grep \"CPU usage\"") ""

processRawCPUData :: Parser RawCPU
processRawCPUData =
  RawCPU
    <$> (P.string "CPU usage:" *> P.char ' ' *> P.double <* P.string "% user,")
    <*> (P.char ' ' *> P.double <* P.string "% sys,")
    <*> (P.char ' ' *> P.double <* P.string "% idle" <* many (P.satisfy (\c -> c == ' ' || c == '\n')))

rawToFull :: RawCPU -> ProcessedCPU
rawToFull r =
  ProcessedCPU (user r + sys r) (user r) (sys r) (idle r)

getCPUStats :: IO (Either String ProcessedCPU)
getCPUStats = do
  rawData <- getRawCPUData
  return $ rawToFull <$> P.parse processRawCPUData rawData

-- | Run and print raw CPU stats
printRawCPUStats :: IO ()
printRawCPUStats = do
  rawData <- getRawCPUData
  putStrLn $ "Raw data: '" ++ rawData ++ "'"
  case P.parse processRawCPUData rawData of
    Right rawCpu -> print $ rawToFull rawCpu
    Left err -> putStrLn err
