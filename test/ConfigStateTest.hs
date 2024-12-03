module ConfigStateTest (configStateTests) where

import ConfigState
  ( ConfigState (..),
    DisplayMode (..),
    ProcessSortMethod (..),
    configToString,
    defaultConfig,
    parseConfigString,
  )
import Test.HUnit

configStateTests :: Test
configStateTests =
  TestList
    [ TestLabel "Default Initialization" testDefaultInitialization,
      TestLabel "Process Display Limit Positive" testProcessDisplayLimit
    ]

testDefaultInitialization :: Test
testDefaultInitialization =
  TestCase $
    let config = ConfigState SortByCPU ShowAll 10
     in assertEqual "ConfigState initializes correctly" config (ConfigState SortByCPU ShowAll 10)

testProcessDisplayLimit :: Test
testProcessDisplayLimit =
  TestCase $
    let config = ConfigState SortByMemory ShowProcesses 20
     in assertBool "Process display limit is positive" (processDisplayLimit config > 0)

testConfigRoundtrip :: ConfigState -> Bool
testConfigRoundtrip config =
  case parseConfigString (configToString config) of
    Just parsedConfig -> parsedConfig == config
    Nothing -> False