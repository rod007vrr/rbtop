{-# LANGUAGE ImportQualifiedPost #-}

module UserSettingsTest (runUserSettingsTests, userSettingsTests) where

import Control.Applicative
import Parser
import Parser qualified as P
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.HUnit
import UserSettings

userSettingsTests :: Test
userSettingsTests =
  TestList
    [ TestLabel "Save and load user settings" testSaveLoadSettings,
      TestLabel "Parse user settings" testParseUserSettings
    ]

testSaveLoadSettings :: Test
testSaveLoadSettings =
  TestCase $ do
    let settings = UserSettings SortPID CpuPct LeftRight
    withSystemTempFile "settings.txt" $ \path handle -> do
      hClose handle
      saveUserSettings settings path
      loaded <- loadUserSettings path
      assertEqual "Loaded settings should match saved settings" (Just settings) loaded

testParseUserSettings :: Test
testParseUserSettings =
  TestCase $ do
    let input = "tableSort=SortPID\nselectedGraph=CpuPct"
        expected = UserSettings SortPID CpuPct LeftRight
    case parse userSettingsP input of
      Left err -> assertFailure $ "Failed to parse valid settings: " ++ show err
      Right actual -> assertEqual "Parsed settings should match expected" expected actual

runUserSettingsTests :: IO Counts
runUserSettingsTests = runTestTT userSettingsTests
