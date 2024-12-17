module UI.UIManager where

import Brick.Main
import Brick.Types ( EventM, BrickEvent(VtyEvent), Widget )
import Brick.AttrMap
import Graphics.Vty
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import UI.Graph
import UI.Table (tableWidget)
import qualified Data.Vector.Unboxed as V



ui :: IO ()
ui = do
    initialState <- buildInitialState
    endState <- defaultMain (app :: App UIState e ResourceName) initialState
    print endState

buildInitialState :: IO UIState
buildInitialState = do
    initialGraph <- initializeGraph 1
    pure UIState {processData = mockData, cpuGraphData = Just initialGraph}

data Process = Process {
    pid :: Int,
    name :: String,
    cpuPercent :: Double,
    memoryPercent :: Double
} deriving (Show, Eq)

mockData :: [Process]
mockData = [
    Process { pid = 1, name = "bash", cpuPercent = 0.1, memoryPercent = 0.2 },
    Process { pid = 2, name = "vim", cpuPercent = 0.3, memoryPercent = 0.4 },
    Process { pid = 3, name = "emacs", cpuPercent = 0.5, memoryPercent = 0.6 },
    Process { pid = 4, name = "less", cpuPercent = 0.7, memoryPercent = 0.8 },
    Process { pid = 5, name = "top", cpuPercent = 0.9, memoryPercent = 1.0 },
    Process { pid = 6, name = "htop", cpuPercent = 1.1, memoryPercent = 1.2 }
    ]

type ResourceName = String

data UIState = UIState
    { processData :: [Process],
    cpuGraphData :: Maybe GraphData,
    memGraphData :: Maybe GraphData } deriving (Show, Eq)

app :: App UIState e ResourceName
app = App {
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return (),
    appAttrMap = const $ attrMap defAttr [(headerAttr, bg blue)]
}

headerAttr :: AttrName
headerAttr = attrName "header"

drawUI :: UIState -> [Widget ResourceName]
drawUI s = [vBox
    [ hBox
        [ padRight (Pad 2) $ renderTable s
        , legendBox
        ]
        , maybe emptyWidget renderGraph (cpuGraphData s)
    ]]

legendBox :: Widget ResourceName
legendBox = borderWithLabel (str "Controls") $
    vBox [ str "q - quit"
         , str "s - sort"
         , str "g - show graph"
         ]

makeRow :: Process -> [String]
makeRow p = [show $ pid p, name p, show $ cpuPercent p, show $ memoryPercent p]

renderTable :: UIState -> Widget ResourceName
renderTable s = tableWidget headers rows
    where
        headers = ["PID", "Name", "CPU %", "Memory %"]
        rows = map makeRow (processData s)

renderGraph :: GraphData -> Widget ResourceName
renderGraph graphData =
  let thinBars = map (`renderThinBar` 30) (V.toList (points graphData))
  in borderWithLabel (str $ "CPU Usage - PID " ++ show (processId graphData)) $
       hBox $ map (padLeft (Pad 0) . padRight (Pad 0)) thinBars

handleEvent :: BrickEvent n e -> EventM n UIState ()
handleEvent e = case e of
    VtyEvent vtye ->
        case vtye of
            EvKey (KChar 'q') [] -> halt
            _ -> return ()
    _ -> return ()

