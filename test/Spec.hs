module Main (main) where

import CPUCollectorTest
import ConfigStateTest
import MemoryCollectorTest
import ProcessCollectorTest
import SystemStateTest
import Test.HUnit

main :: IO ()
main = do
  counts <-
    runTestTT $
      TestList
        [ configStateTests,
          cpuCollectorTests,
          memoryCollectorTests,
          processCollectorTests,
          systemStateTests
        ]
  print counts