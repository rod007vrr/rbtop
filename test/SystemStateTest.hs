module SystemStateTest (systemStateTests) where

import CPUCollector
import MemoryCollector
import SystemState
import Test.HUnit

systemStateTests :: Test
systemStateTests =
  TestList
    [TestLabel "Initialize SystemState" testInitializeSystemState]

testInitializeSystemState :: Test
testInitializeSystemState =
  TestCase $
    let mem = ProcessedMemory 0 16384000 8192000 8192000 2048000 1024000
        cpu = ProcessedCPU 0 50.0 30.0 15.0 5.0 0.0
        state = SystemState mem cpu []
     in do
          assertEqual "Memory stats match" mem (memoryStats state)
          assertEqual "CPU stats match" cpu (cpuStats state)