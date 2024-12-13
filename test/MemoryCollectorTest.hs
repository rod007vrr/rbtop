module MemoryCollectorTest (memoryCollectorTests) where

import MemoryCollector
import Test.HUnit

memoryCollectorTests :: Test
memoryCollectorTests =
  TestList
    [ TestLabel "Initialize ProcessedMemory" testInitializeProcessedMemory,
      TestLabel "Memory Usage Valid" testMemoryUsageValid
    ]

testInitializeProcessedMemory :: Test
testInitializeProcessedMemory =
  TestCase $
    let mem = ProcessedMemory 0 16384000 8192000 8192000
     in do
          assertEqual "Total memory" 16384000 (totalMem mem)
          assertEqual "Free memory" 8192000 (freeMem mem)

testMemoryUsageValid :: Test
testMemoryUsageValid =
  TestCase $
    let mem = ProcessedMemory 0 16384000 8192000 8192000
     in assertBool "Used + Free <= Total" (usedMem mem + freeMem mem <= totalMem mem)