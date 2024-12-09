{-# LANGUAGE ImportQualifiedPost #-}

module CPUCollector
  ( ProcessedCPU (..),
    ProcessedCPUList,
    getRawCPUData,
    processCPUData,
    parseCPUStats,
    printCPUStats,
  )
where

import Control.Applicative
import Data.Char qualified as Char
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

type ProcessedCPUList = [ProcessedCPU]

getRawCPUData :: IO String
getRawCPUData = readCreateProcess (shell "top -l 1 -n 0 | grep \"CPU usage\"") ""

-- | Print the CPU stats to stdout
processCPUData :: Parser ProcessedCPU
processCPUData =
  ProcessedCPU
    <$> pure 0
    <* P.wsP (P.string "CPU usage:")
    <*> (P.wsP P.double <* P.string "%" <* P.wsP (P.string "user,"))
    <*> (P.wsP P.double <* P.string "%" <* P.wsP (P.string "sys,"))
    <*> (P.wsP P.double <* P.string "%" <* P.wsP (P.string "idle\n"))
    <*> pure 0 -- totalUsage (can calculate after)

-- | Parse CPU stats from raw input string
parseCPUStats :: String -> Maybe ProcessedCPU
parseCPUStats input = case P.parse processCPUData input of
  Right cpu -> Just $ cpu {totalUsage = userUsage cpu + systemUsage cpu}
  Left _ -> Nothing

-- | Run and print CPU stats
printCPUStats :: IO ()
printCPUStats = do
  rawData <- getRawCPUData
  putStrLn $ "Raw data: '" ++ rawData ++ "'" -- Added quotes to see whitespace
  case parseCPUStats rawData of
    Just cpu -> putStrLn $ formatCPUStats cpu
    Nothing -> putStrLn "Failed to parse CPU stats"
  where
    formatCPUStats cpu =
      unlines
        [ "CPU Stats:",
          "  Total Usage: " ++ show (totalUsage cpu) ++ "%",
          "  User Usage: " ++ show (userUsage cpu) ++ "%",
          "  System Usage: " ++ show (systemUsage cpu) ++ "%",
          "  Idle Usage: " ++ show (idleUsage cpu) ++ "%"
        ]
