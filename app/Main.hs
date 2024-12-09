{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Parser (Parser (..))
import Parser qualified as P
import ProcessCollector (getProcessList, printProcessList, processListP)

main :: IO ()
main = forever $ do
  content <- getProcessList
  case content of
    Just ct -> printProcessList ct
    Nothing -> putStrLn "bruh"

  threadDelay 1000000 -- 1 second