{-# LANGUAGE ImportQualifiedPost #-}

module CPUCollector
  ( ProcessedCPU (..),
    ProcessedCPUList,
    getCommandOutput,
    processListP,
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
    idleUsage :: Double,
    -- | IO wait percentage (0-100)
    ioWaitUsage :: Double
  }
  deriving (Show, Eq)

type ProcessedCPUList = [ProcessedCPU]

-- Basic system command (returns ExitCode)

-- Run command and get output as String
getCommandOutput :: IO String
getCommandOutput = readProcess "ps" ["aux"] ""

-- First define a data type to hold the process information
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

-- Parser for a single process line
processLineP :: Parser Process
processLineP =
  Process
    <$> wsP (many (P.satisfy (not . Char.isSpace)))
    <*> wsP P.int
    <*> wsP P.double
    <*> wsP P.double
    <*> wsP P.int
    <*> wsP P.int
    <*> wsP (many (P.satisfy (not . Char.isSpace)))
    <*> wsP (many (P.satisfy (not . Char.isSpace)))
    <*> wsP (many (P.satisfy (not . Char.isSpace)))
    <*> wsP (many (P.satisfy (not . Char.isSpace)))
    <*> wsP (many (P.satisfy (/= '\n')))

-- Parser for multiple process lines
processListP :: Parser [Process]
processListP =
  skipHeaderP *> many processLineP
  where
    skipHeaderP = P.createParser $ \input ->
      case break (== '\n') input of
        (_, rest) -> Just ((), drop 1 rest)

-- | removes trailing whitespace
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- Example usage:
-- P.parse processListP "PID TTY