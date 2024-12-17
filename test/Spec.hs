module Main (main) where

import CPUCollectorTest
import MemoryCollectorTest
import ProcessCollectorTest
import SystemStateTest
import Test.HUnit

main :: IO ()
main = do
  counts <-
    runTestTT $
      TestList
        [ -- cpuCollectorTests,
          memoryCollectorTests
          -- processCollectorTests,
          -- systemStateTests
        ]
  print counts