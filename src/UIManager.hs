module UIManager
  ( -- | TBD: Add exports when UI implementation is ready
  )
where

import CPUCollector (ProcessedCPU)
import ConfigState (ConfigState (..), DisplayMode (..), ProcessSortMethod (..))
import MemoryCollector (ProcessedMemory)
import ProcessCollector (Process)

-- | Represents the rendered UI elements and their data
data UIState = UIState
  { -- | Current configuration state
    configState :: ConfigState,
    -- | Memory information being displayed
    displayedMemory :: ProcessedMemory,
    -- | CPU information being displayed
    displayedCPU :: ProcessedCPU,
    -- | List of processes being displayed
    displayedProcesses :: [Process],
    -- | Whether the help dialog is visible
    showingHelp :: Bool,
    -- | Whether the quit confirmation dialog is visible
    showingQuitDialog :: Bool
  }

-- | Render the UI state to the terminal
renderUI :: UIState -> IO ()
renderUI state = undefined

-- TODO: Implement UI rendering based on:
-- - configState state (for sort method, display mode, process limit)
-- - displayedMemory state (for memory stats)
-- - displayedCPU state (for CPU stats)
-- - displayedProcesses state (for process list)
-- - showingHelp state (for help dialog)
-- - showingQuitDialog state (for quit confirmation)
