{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import ProcessCollector (getProcessList, printProcessList, processListP)

main :: IO ()
main = forever $ do
  putStrLn "bruh"

  threadDelay 1000000 -- 1 second