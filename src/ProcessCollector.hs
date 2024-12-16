{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module ProcessCollector
  ( Process (..),
    ProcessList,
    sortByCPU,
    sortByMemory,
    sortByPID,
    sortByName,
    processListP,
    printProcessList,
    getProcessList,
    sampleProcesses,
  )
where

import Control.Applicative
import Data.Char qualified as Char
import Data.Function
import Data.List
-- import Data.Time.Clock
import Parser (Parser)
import Parser qualified as P
import System.Process (readProcess)
import Prelude hiding (filter)

sampleProcesses :: [Process]
sampleProcesses =
  [ Process "root" 1 0.0 0.1 12345 2048 "??" "S" "Apr10" "0:01" "/sbin/init",
    Process "alice" 1234 25.3 5.2 45678 8192 "ttys001" "R+" "10:30" "1:23" "firefox",
    Process "bob" 2345 15.7 3.8 34567 4096 "ttys002" "S+" "11:45" "0:45" "chrome",
    Process "daemon" 321 0.5 0.3 23456 1024 "??" "Ss" "Apr09" "0:30" "/usr/sbin/cron",
    Process "www-data" 3456 8.2 2.1 56789 3072 "??" "S" "12:15" "2:10" "nginx: worker"
  ]

type ProcessList = [Process]

data Process = Process
  { user :: String,
    pid :: Int,
    cpuPct :: Double,
    memPct :: Double,
    vsz :: Int,
    rss :: Int,
    tt :: String,
    stat :: String,
    started :: String,
    time :: String,
    cmd :: String
  }
  deriving (Show, Eq)

-- Sample process data

getRawProcessData :: IO String
getRawProcessData = readProcess "ps" ["aux"] ""

-- | Attempts to parse raw process data and returns Either an error message or ProcessList
parseProcessData :: String -> Maybe ProcessList
parseProcessData rawData =
  case P.parse processListP rawData of
    Right procs -> Just procs
    Left _ -> Nothing

-- | Gets process list from system and returns Either an error message or ProcessList
getProcessList :: IO (Maybe ProcessList)
getProcessList = parseProcessData <$> getRawProcessData

-- | Print the process list to stdout
printProcessList :: ProcessList -> IO ()
printProcessList = mapM_ print

-- Parser for a single process line
processLineP :: Parser Process
processLineP =
  Process
    <$> P.wsP (many (P.satisfy (not . Char.isSpace)))
    <*> P.wsP P.int
    <*> P.wsP P.double
    <*> P.wsP P.double
    <*> P.wsP P.int
    <*> P.wsP P.int
    <*> P.wsP (many (P.satisfy (not . Char.isSpace)))
    <*> P.wsP (many (P.satisfy (not . Char.isSpace)))
    <*> P.wsP (many (P.satisfy (not . Char.isSpace)))
    <*> P.wsP (many (P.satisfy (not . Char.isSpace)))
    <*> P.wsP (many (P.satisfy (/= '\n')))

-- Parser for multiple process lines
processListP :: Parser [Process]
processListP =
  skipHeaderP *> many processLineP
  where
    skipHeaderP = P.createParser $ \input ->
      case break (== '\n') input of
        (_, rest) -> Just ((), drop 1 rest)

-- | removes trailing whitespace

-- | Sort processes by CPU usage (descending)
sortByCPU :: [Process] -> [Process]
sortByCPU = sortBy (flip compare `on` cpuPct)

-- | Sort processes by memory usage (descending)
sortByMemory :: [Process] -> [Process]
sortByMemory = sortBy (flip compare `on` memPct)

-- | Sort processes by PID (ascending)
sortByPID :: [Process] -> [Process]
sortByPID = sortBy (compare `on` pid)

-- | Sort processes by name (ascending)
sortByName :: [Process] -> [Process]
sortByName = sortBy (compare `on` cmd)
