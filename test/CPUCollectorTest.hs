{-# LANGUAGE ImportQualifiedPost #-}

module CPUCollectorTest (cpuCollectorTests) where

import CPUCollector
import Parser qualified as P
import Test.HUnit

cpuCollectorTests :: Test
cpuCollectorTests =
  TestList
    [ TestLabel "Test ProcessedCPU creation" testProcessedCPU,
      TestLabel "Test raw CPU parsing" testRawCPUParsing,
      TestLabel "Test raw to processed conversion" testRawToProcessed
    ]

testProcessedCPU :: Test
testProcessedCPU =
  TestCase $ do
    let cpu = ProcessedCPU 80.0 50.0 30.0 20.0
    assertEqual "Total CPU usage" 80.0 (totalUsage cpu)
    assertEqual "User CPU usage" 50.0 (userUsage cpu)
    assertEqual "System CPU usage" 30.0 (systemUsage cpu)
    assertEqual "Idle CPU usage" 20.0 (idleUsage cpu)

testRawCPUParsing :: Test
testRawCPUParsing =
  TestCase $ do
    let input = "CPU usage: 50.0% user, 30.0% sys, 20.0% idle\n"
    case P.parse processRawCPUData input of
      Right rawCpu -> do
        assertEqual "User CPU" 50.0 (user rawCpu)
        assertEqual "System CPU" 30.0 (sys rawCpu)
        assertEqual "Idle CPU" 20.0 (idle rawCpu)
      Left err -> assertFailure $ "Failed to parse raw CPU data: " ++ err

testRawToProcessed :: Test
testRawToProcessed =
  TestCase $ do
    let rawCpu = RawCPU 50.0 30.0 20.0
        processed = rawToFull rawCpu
    assertEqual "Total usage" 80.0 (totalUsage processed)
    assertEqual "User usage" 50.0 (userUsage processed)
    assertEqual "System usage" 30.0 (systemUsage processed)
    assertEqual "Idle usage" 20.0 (idleUsage processed)