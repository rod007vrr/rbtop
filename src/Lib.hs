module Lib
  ( parseMemoryStats,
    parseCPUStats,
    parseProcessStats,
    updateSystemState,
    formatMemoryStats,
    formatCPUStats,
    formatProcessStats,
  )
where

import qualified CPUCollector as CPU
import Data.List (intercalate)
import qualified MemoryCollector as Memory
import qualified ProcessCollector as Process
import SystemState (SystemState (..))

somefunc :: IO ()
somefunc = putStrLn "somefunc"

-- | Parses a memory usage string into a ProcessedMemory object
-- Example input format: "0 16384000 8192000 8192000 2048000 1024000"
parseMemoryStats :: String -> Memory.ProcessedMemory
parseMemoryStats input =
  let [timestamp, total, used, free, cached, buffers] = map read (words input)
   in Memory.ProcessedMemory timestamp total used free cached buffers

-- | Parses a CPU usage string into a ProcessedCPU object
-- Example input format: "0 50.0 30.0 15.0 5.0 0.0"
parseCPUStats :: String -> CPU.ProcessedCPU
parseCPUStats input =
  let [timestampStr, totalStr, userStr, systemStr, idleStr, ioWaitStr] = words input
      timestamp = read timestampStr :: Integer
      total = read totalStr :: Double
      user = read userStr :: Double
      system = read systemStr :: Double
      idle = read idleStr :: Double
      ioWait = read ioWaitStr :: Double
   in CPU.ProcessedCPU timestamp total user system idle ioWait

-- | Parses a process usage string into a ProcessedProcess object
-- Example input format: "0 1234 process_name 10.0 204800 102400 51200"
parseProcessStats :: String -> Process.ProcessedProcess
parseProcessStats input =
  let [timestamp, pid, name, cpuUsage, memUsage, virtMem, resMem] = words input
   in Process.ProcessedProcess (read timestamp) (read pid) name (read cpuUsage) (read memUsage) (read virtMem) (read resMem)

-- | Advances the system state by one step, e.g., to simulate time updates
updateSystemState :: SystemState -> SystemState
updateSystemState (SystemState memory cpu processes) =
  SystemState
    { memoryStats = memory {Memory.timestamp = Memory.timestamp memory + 1},
      cpuStats = cpu {CPU.timestamp = CPU.timestamp cpu + 1},
      processStats = map (\p -> p {Process.timestamp = Process.timestamp p + 1}) processes
    }

-- | Formats memory statistics into a human-readable string
formatMemoryStats :: Memory.ProcessedMemory -> String
formatMemoryStats mem =
  unlines
    [ "Memory Stats:",
      "  Total: " ++ show (Memory.totalMem mem) ++ " bytes",
      "  Used: " ++ show (Memory.usedMem mem) ++ " bytes",
      "  Free: " ++ show (Memory.freeMem mem) ++ " bytes",
      "  Cached: " ++ show (Memory.cached mem) ++ " bytes",
      "  Buffers: " ++ show (Memory.buffers mem) ++ " bytes"
    ]

-- | Formats CPU statistics into a human-readable string
formatCPUStats :: CPU.ProcessedCPU -> String
formatCPUStats cpu =
  unlines
    [ "CPU Stats:",
      "  Total Usage: " ++ show (CPU.totalUsage cpu) ++ "%",
      "  User Usage: " ++ show (CPU.userUsage cpu) ++ "%",
      "  System Usage: " ++ show (CPU.systemUsage cpu) ++ "%",
      "  Idle Usage: " ++ show (CPU.idleUsage cpu) ++ "%",
      "  IO Wait: " ++ show (CPU.ioWaitUsage cpu) ++ "%"
    ]

-- | Formats a list of processes into a human-readable string
formatProcessStats :: Process.ProcessedProcessList -> String
formatProcessStats processes =
  unlines $ "Processes:" : map formatProcess processes
  where
    formatProcess p =
      intercalate
        " | "
        [ "PID: " ++ show (Process.pid p),
          "Name: " ++ Process.name p,
          "CPU Usage: " ++ show (Process.cpuPercent p) ++ "%",
          "Memory Usage: " ++ show (Process.memoryUsage p) ++ " bytes"
        ]