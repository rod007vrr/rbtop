{-# LANGUAGE ImportQualifiedPost #-}

module MemoryCollectorTest (memoryCollectorTests) where

import MemoryCollector
  ( ProcessedMemory (freeMem, freeMemPercent, totalMem, usedMem),
    RawMemory (..),
    processMemoryData,
    toProcessedMemory,
  )
import Parser qualified as P
import Test.HUnit

-- Sample memory pressure output for testing
sampleMemoryOutput :: String
sampleMemoryOutput =
  unlines
    [ "The system has 17179869184 (4194304 pages with a page size of 4096).",
      "",
      "Stats: ",
      "Pages free: 123456 ",
      "Pages purgeable: 7890 ",
      "Pages purged: 1234 ",
      "",
      "Swap I/O:",
      "Swapins: 0 ",
      "Swapouts: 0 ",
      "",
      "Page Q counts:",
      "Pages active: 567890 ",
      "Pages inactive: 234567 ",
      "Pages speculative: 12345 ",
      "Pages throttled: 0 ",
      "Pages wired down: 345678 ",
      "",
      "Compressor Stats:",
      "Pages used by compressor: 45678 ",
      "Pages decompressed: 1234 ",
      "Pages compressed: 5678 ",
      "",
      "File I/O:",
      "Pageins: 89012 ",
      "Pageouts: 3456 ",
      "",
      "System-wide memory free percentage: 45.67%"
    ]

testParseMemoryData :: Test
testParseMemoryData = TestCase $ do
  let result = P.parse processMemoryData sampleMemoryOutput
  case result of
    Right rawMemory -> do
      assertEqual "Total memory size" 17179869184 (totalMemSize rawMemory)
      assertEqual "Total pages" 4194304 (totalPages rawMemory)
      assertEqual "Page size" 4096 (pageSize rawMemory)
      assertEqual "Free pages" 123456 (freePages rawMemory)
      assertEqual "Free memory percentage" 45.67 (memFreePercent rawMemory)
    Left err -> assertFailure $ "Failed to parse memory data: " ++ err

testProcessedMemoryConversion :: Test
testProcessedMemoryConversion = TestCase $ do
  let result = P.parse processMemoryData sampleMemoryOutput
  case result of
    Right rawMemory -> do
      let processed = toProcessedMemory rawMemory
      assertEqual "Total memory" 17179869184 (totalMem processed)
      assertEqual "Free memory" (123456 * 4096) (freeMem processed)
      assertEqual "Used memory" (17179869184 - (123456 * 4096)) (usedMem processed)
      assertEqual "Free memory percentage" 45.67 (freeMemPercent processed)
    Left err -> assertFailure $ "Failed to parse memory data: " ++ err

memoryCollectorTests :: Test
memoryCollectorTests =
  TestList
    [ TestLabel "Parse Memory Data" testParseMemoryData,
      TestLabel "Process Memory Conversion" testProcessedMemoryConversion
    ]
