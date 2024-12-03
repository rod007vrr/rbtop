module ConfigState
  ( ConfigState (..),
    ProcessSortMethod (..),
    DisplayMode (..),
    parseConfigString,
    configToString,
    defaultConfig,
  )
where

-- | How processes should be sorted in the display
data ProcessSortMethod
  = -- | Sort by CPU usage (descending)
    SortByCPU
  | -- | Sort by memory usage (descending)
    SortByMemory
  | -- | Sort by process ID (ascending)
    SortByPID
  | -- | Sort by process name (ascending)
    SortByName
  deriving (Show, Eq)

-- | Display mode for the UI
data DisplayMode
  = -- | Show both process and memory information
    ShowAll
  | -- | Show only process information
    ShowProcesses
  | -- | Show only memory information
    ShowMemory
  deriving (Show, Eq)

-- | Configuration state for the application
data ConfigState = ConfigState
  { -- | Current sort method for processes
    processSortMethod :: ProcessSortMethod,
    -- | Current display mode
    displayMode :: DisplayMode,
    -- | Number of processes to display
    processDisplayLimit :: Int
  }
  deriving (Show, Eq)

defaultConfig :: ConfigState
defaultConfig =
  ConfigState
    { processSortMethod = SortByCPU,
      displayMode = ShowAll,
      processDisplayLimit = 10
    }

-- | Convert ConfigState to a string representation
configToString :: ConfigState -> String
configToString config =
  unlines
    [ show (processSortMethod config),
      show (displayMode config),
      show (processDisplayLimit config)
    ]

-- | Write configuration to a file
writeConfig :: FilePath -> ConfigState -> IO ()
writeConfig path config = writeFile path $ configToString config

-- | Parse configuration from string representation
-- Returns Nothing if parsing fails
parseConfigString :: String -> Maybe ConfigState
parseConfigString input = do
  let ls = lines input
  case ls of
    [sortStr, filterStr, limitStr] -> do
      sort <- readSortMethod sortStr
      mode <- readDisplayMode filterStr
      limit <- readMaybe limitStr
      return $ ConfigState sort mode limit
    _ -> Nothing

-- | Test that parsing a config string after converting to string returns the original config
testConfigRoundtrip :: ConfigState -> Bool
testConfigRoundtrip config =
  case parseConfigString (configToString config) of
    Just parsedConfig -> parsedConfig == config
    Nothing -> False

-- | Read configuration from a file
-- Returns Nothing if parsing fails
readConfig :: FilePath -> IO (Maybe ConfigState)
readConfig path = do
  contents <- readFile path
  return $ parseConfigString contents

-- | Parse ProcessSortMethod from string
readSortMethod :: String -> Maybe ProcessSortMethod
readSortMethod "SortByCPU" = Just SortByCPU
readSortMethod "SortByMemory" = Just SortByMemory
readSortMethod "SortByPID" = Just SortByPID
readSortMethod "SortByName" = Just SortByName
readSortMethod _ = Nothing

-- | Parse DisplayMode from string
readDisplayMode :: String -> Maybe DisplayMode
readDisplayMode "ShowAll" = Just ShowAll
readDisplayMode "ShowProcesses" = Just ShowProcesses
readDisplayMode "ShowMemory" = Just ShowMemory
readDisplayMode _ = Nothing

-- | Helper function to safely read an Int
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing
