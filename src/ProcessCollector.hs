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

type ProcessList = [Process]

data ProcessListAtTime = ProcessListAtTime
  { processes :: ProcessList,
    systemTime :: String
  }

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

getRawProcessData :: IO String
getRawProcessData = readProcess "ps" ["aux"] ""

constructProcessListAtTime :: String -> Maybe ProcessListAtTime
constructProcessListAtTime rawData =
  case P.parse processListP rawData of
    Right procs -> do
      let currentTime = "temp"
      Just $ ProcessListAtTime procs currentTime
    Left _ -> Nothing

getProcessList :: IO (Maybe ProcessListAtTime)
getProcessList = do constructProcessListAtTime <$> getRawProcessData

-- | Print the process list to stdout
printProcessList :: ProcessListAtTime -> IO ()
printProcessList procList = mapM_ print (processes procList)

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
