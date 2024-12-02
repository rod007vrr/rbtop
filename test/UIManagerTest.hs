module UIManagerTest (uiManagerTests) where

import ConfigState
import Test.HUnit
import UIManager

uiManagerTests :: Test
uiManagerTests =
  TestList
    [TestLabel "Default UI State" testDefaultUIState]

testDefaultUIState :: Test
testDefaultUIState =
  TestCase $
    let config = ConfigState SortByCPU ShowAll 10
        uiState = UIState config undefined undefined [] False False
     in assertBool "UI initialized without errors" (not (showingHelp uiState))