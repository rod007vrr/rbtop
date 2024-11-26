module ConfigState
  ( ConfigState (..),
    ProcessSortMethod (..),
    DisplayMode (..),
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
