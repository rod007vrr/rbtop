module CPUCollectorTest () where

import CPUCollector
import Test.HUnit

-- cpuCollectorTests :: Test
-- cpuCollectorTests =
--   TestList
--     [TestLabel "Initialize ProcessedCPU" testInitializeProcessedCPU]

-- testInitializeProcessedCPU :: Test
-- testInitializeProcessedCPU =
--   TestCase $
--     let cpu = ProcessedCPU 0 50.0 30.0 15.0 5.0 0.0
--      in do
--           assertEqual "Total CPU usage" 50.0 (totalUsage cpu)
--           assertEqual "Idle CPU usage" 5.0 (idleUsage cpu)