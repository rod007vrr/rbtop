module ProcessCollectorTest (processCollectorTests) where

import ProcessCollector
import Test.HUnit
import Test.QuickCheck

-- Test data
testProcesses :: [Process]
testProcesses =
  [ Process "user1" 100 50.0 30.0 1000 500 "tt1" "S" "10:00" "1:00" "proc1",
    Process "user2" 200 20.0 10.0 2000 600 "tt2" "R" "11:00" "2:00" "proc2",
    Process "user3" 300 30.0 20.0 3000 700 "tt3" "S" "12:00" "3:00" "proc3"
  ]

-- HUnit Tests
testSortByCPU :: Test
testSortByCPU = TestCase $ do
  let sorted = sortByCPU testProcesses
  assertEqual "Sort by CPU" 50.0 (cpuPct $ head sorted)
  assertEqual "Sort by CPU (last)" 20.0 (cpuPct $ last sorted)

testSortByMemory :: Test
testSortByMemory = TestCase $ do
  let sorted = sortByMemory testProcesses
  assertEqual "Sort by Memory" 30.0 (memPct $ head sorted)
  assertEqual "Sort by Memory (last)" 10.0 (memPct $ last sorted)

testSortByPID :: Test
testSortByPID = TestCase $ do
  let sorted = sortByPID testProcesses
  assertEqual "Sort by PID" 100 (pid $ head sorted)
  assertEqual "Sort by PID (last)" 300 (pid $ last sorted)

testSortByName :: Test
testSortByName = TestCase $ do
  let sorted = sortByName testProcesses
  assertEqual "Sort by Name" "proc1" (cmd $ head sorted)
  assertEqual "Sort by Name (last)" "proc3" (cmd $ last sorted)

-- QuickCheck Properties
instance Arbitrary Process where
  arbitrary = do
    usr <- elements ["user1", "user2", "user3", "user4"]
    pid' <- choose (1, 10000)
    cpu <- choose (0.0, 100.0)
    mem <- choose (0.0, 100.0)
    vsz' <- choose (1000, 10000)
    rss' <- choose (100, 1000)
    tt' <- elements ["tt1", "tt2", "tt3"]
    stat' <- elements ["S", "R", "Z"]
    started' <- elements ["10:00", "11:00", "12:00"]
    time' <- elements ["0:01", "1:00", "2:00"]
    cmd' <- elements ["proc1", "proc2", "proc3", "proc4"]
    return $ Process usr pid' cpu mem vsz' rss' tt' stat' started' time' cmd'

prop_sortByCPUMaintainsLength :: [Process] -> Bool
prop_sortByCPUMaintainsLength ps = length (sortByCPU ps) == length ps

prop_sortByMemoryMaintainsLength :: [Process] -> Bool
prop_sortByMemoryMaintainsLength ps = length (sortByMemory ps) == length ps

prop_sortByCPUOrdering :: [Process] -> Property
prop_sortByCPUOrdering ps =
  not (null ps) ==>
    let sorted = sortByCPU ps
     in all (\(p1, p2) -> cpuPct p1 >= cpuPct p2) $ zip sorted (tail sorted)

prop_sortByMemoryOrdering :: [Process] -> Property
prop_sortByMemoryOrdering ps =
  not (null ps) ==>
    let sorted = sortByMemory ps
     in all (\(p1, p2) -> memPct p1 >= memPct p2) $ zip sorted (tail sorted)

-- Combine all tests
processCollectorTests :: Test
processCollectorTests =
  TestList
    [ TestLabel "Sort by CPU Test" testSortByCPU,
      TestLabel "Sort by Memory Test" testSortByMemory,
      TestLabel "Sort by PID Test" testSortByPID,
      TestLabel "Sort by Name Test" testSortByName
    ]
