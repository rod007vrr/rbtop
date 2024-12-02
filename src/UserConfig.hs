module UserConfig
  ( configToString,
    parseConfigString,
  )
where

-- | Represents how processes should be sorted in the display
data SortCriteria
  = -- | Sort by CPU usage
    SortByCPU
  | -- | Sort by memory usage
    SortByMemory
  | -- | Sort by process ID
    SortByPID
  deriving (Show, Eq)

-- | Determines what processes should be shown
data DisplayFilter
  = -- | Show all processes
    ShowAll
  | -- | Show only user processes
    ShowProcesses
  | -- | Show only system processes
    ShowSystem
  deriving (Show, Eq)

-- | Main configuration type for the monitoring application
data ConfigState = ConfigState
  { -- | How to sort the process list
    sortBy :: SortCriteria,
    -- | Which processes to display
    displayFilter :: DisplayFilter,
    -- | Maximum number of processes to show
    processDisplayLimit :: Int
  }
  deriving (Show, Eq)

-- | Default configuration
defaultConfig :: ConfigState
defaultConfig =
  ConfigState
    { sortBy = SortByCPU,
      displayFilter = ShowAll,
      processDisplayLimit = 10
    }

-- | Write configuration to a file
-- | Convert ConfigState to a string representation
configToString :: ConfigState -> String
configToString config =
  unlines
    [ show (sortBy config),
      show (displayFilter config),
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
      sort <- readSortCriteria sortStr
      filt <- readDisplayFilter filterStr
      limit <- readMaybe limitStr
      return $ ConfigState sort filt limit
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

-- | Parse SortCriteria from string
readSortCriteria :: String -> Maybe SortCriteria
readSortCriteria "SortByCPU" = Just SortByCPU
readSortCriteria "SortByMemory" = Just SortByMemory
readSortCriteria "SortByPID" = Just SortByPID
readSortCriteria _ = Nothing

-- | Parse DisplayFilter from string
readDisplayFilter :: String -> Maybe DisplayFilter
readDisplayFilter "ShowAll" = Just ShowAll
readDisplayFilter "ShowProcesses" = Just ShowProcesses
readDisplayFilter "ShowSystem" = Just ShowSystem
readDisplayFilter _ = Nothing

-- | Helper function to safely read an Int
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing
