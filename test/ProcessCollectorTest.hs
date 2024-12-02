module ProcessCollectorTest (processCollectorTests) where

import ProcessCollector
import Test.HUnit
import Test.QuickCheck

processCollectorTests :: Test
processCollectorTests =
  TestList
    [ TestLabel "Sort by CPU" testSortByCPU,
      TestLabel "Sort by Memory" testSortByMemory
    ]

testSortByCPU :: Test
testSortByCPU =
  TestCase $
    let processes =
          [ ProcessedProcess 0 1 "proc1" 20.0 1000 5000 2000,
            ProcessedProcess 0 2 "proc2" 50.0 2000 6000 3000
          ]
        sorted = sortByCPU processes
     in assertEqual "Sorted by CPU descending" [50.0, 20.0] (map cpuPercent sorted)

testSortByMemory :: Test
testSortByMemory =
  TestCase $
    let processes =
          [ ProcessedProcess 0 1 "proc1" 20.0 1000 5000 2000,
            ProcessedProcess 0 2 "proc2" 50.0 2000 6000 3000
          ]
        sorted = sortByMemory processes
     in assertEqual "Sorted by memory descending" [2000, 1000] (map memoryUsage sorted)

-- TODO: no need for not null 
-- | Properties for testing sort functions
prop_sortByCPUDesc :: [ProcessedProcess] -> Property
prop_sortByCPUDesc ps =
  not (null ps) ==>
    let sorted = sortByCPU ps
     in all (\(a, b) -> cpuPercent a >= cpuPercent b) $ zip sorted (tail sorted)

prop_sortByMemoryDesc :: [ProcessedProcess] -> Property
prop_sortByMemoryDesc ps =
  not (null ps) ==>
    let sorted = sortByMemory ps
     in all (\(a, b) -> memoryUsage a >= memoryUsage b) $ zip sorted (tail sorted)

prop_sortByPIDAsc :: [ProcessedProcess] -> Property
prop_sortByPIDAsc ps =
  not (null ps) ==>
    let sorted = sortByPID ps
     in all (\(a, b) -> pid a <= pid b) $ zip sorted (tail sorted)

prop_sortByNameAsc :: [ProcessedProcess] -> Property
prop_sortByNameAsc ps =
  not (null ps) ==>
    let sorted = sortByName ps
     in all (\(a, b) -> name a <= name b) $ zip sorted (tail sorted)