module Main (main) where

import CPUCollectorTest (cpuCollectorTests)
import MemoryCollectorTest (memoryCollectorTests)
import ProcessCollectorTest (processCollectorTests)
import Test.HUnit
import UserSettingsTest (userSettingsTests)

main :: IO ()
main = do
  counts <-
    runTestTT $
      TestList
        [ userSettingsTests,
          cpuCollectorTests,
          memoryCollectorTests,
          processCollectorTests
        ]
  print counts