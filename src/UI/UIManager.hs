{-# OPTIONS_GHC -Wno-unused-imports #-}

module UI.UIManager (ui) where

import Brick (customMain, get, Widget (render), hLimit)
import Brick.AttrMap (AttrName, attrMap, attrName)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (App (..), defaultMain, halt, neverShowCursor)
import Brick.Types (BrickEvent (AppEvent, VtyEvent), EventM, Widget, modify)
import Brick.Util (bg)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core
  ( Padding (Pad),
    hBox,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    emptyWidget,
    vLimit
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Function
import Data.List (break, drop, sortBy)
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import Graphics.Vty (Event (EvKey), Key (KChar, KLeft, KRight, KDown, KUp), blue, defAttr, eventChannel)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import ProcessCollector (Process (..), ProcessList)
import SystemState (SystemState (..), gatherSystemState)
import UI.Graph (GraphData (points, maxPoints, GraphData), renderThinBar)
import UI.Table (tableWidget)
import UserSettings (GraphOptions (..), SortColumn (..))
import MemoryCollector (ProcessedMemory (totalMem, usedMem))
import CPUCollector (ProcessedCPU (totalUsage))
import qualified Data.Maybe
import Brick.Widgets.Center (hCenter)

headerAttr :: AttrName
headerAttr = attrName "header"

type ResourceName = String


-- data PaneFocus = ProcessesPane | LegendPane | MemGraphPane | CPUGraphPane
--   deriving (Eq, Show)

data PaneOrientation = LeftRight | RightLeft | TopBottom | BottomTop deriving (Show, Eq)

data UIState = UIState
  { systemState :: SystemState,
    cpuGraphData :: Maybe GraphData,
    memGraphData :: Maybe GraphData,
    awaitingKey :: Bool,
    tableSort :: SortColumn,
    selectedGraph :: GraphOptions,
    orientation :: PaneOrientation
  }
  deriving (Show, Eq)

data CustomEvent = Tick

ui :: IO ()
ui = do
  chan <- newBChan 10
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000 -- 1 second delay
  initialState <- buildInitialState

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  endState <- customMain initialVty buildVty (Just chan) app initialState

  print endState

buildInitialState :: IO UIState
buildInitialState = do
  maybeSystemState <- gatherSystemState
  pure $ case maybeSystemState of
    Just sysState ->
      UIState
        { systemState = sysState,
          cpuGraphData = Nothing,
          memGraphData = Nothing,
          awaitingKey = False,
          tableSort = SortCPU,
          selectedGraph = CpuPct,
          orientation = LeftRight
        }
    Nothing ->
      UIState
        { systemState = emptySystemState,
          cpuGraphData = Nothing,
          memGraphData = Nothing,
          awaitingKey = False,
          tableSort = SortCPU,
          selectedGraph = CpuPct,
          orientation = LeftRight
        }
  where
    -- Placeholder empty state when we can't get system data
    emptySystemState =
      SystemState
        { memoryStats = error "No memory stats available",
          cpuStats = error "No CPU stats available",
          processStats = []
        }

app :: App UIState CustomEvent ResourceName
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr [(headerAttr, bg blue)]
    }

drawUI :: UIState -> [Widget ResourceName]
drawUI s = case orientation s of
  LeftRight -> [hBox [processesPane s, additionalPane s]]
  RightLeft -> [hBox [additionalPane s, processesPane s]]
  TopBottom -> [vBox [additionalPane s, processesPane s]]
  BottomTop -> [vBox [processesPane s, additionalPane s]]

processesPane :: UIState -> Widget ResourceName
processesPane = renderProcesses

additionalPane :: UIState -> Widget ResourceName
additionalPane s = case orientation s of
  LeftRight -> vBox
    [ legendBox,
      padTop (Pad 1) $
        renderMemoryGraph s,
      padTop (Pad 1) $
        renderCPUGraph s
    ]
  RightLeft -> vBox
    [ legendBox,
      padTop (Pad 1) $
        renderMemoryGraph s,
      padTop (Pad 1) $
        renderCPUGraph s
    ]
  TopBottom -> hBox
    [ legendBox,
      padLeft (Pad 1) $
        renderMemoryGraph s,
      padLeft (Pad 1) $
        renderCPUGraph s
    ]
  BottomTop -> hBox
    [ legendBox,
      padLeft (Pad 1) $
        renderMemoryGraph s,
      padLeft (Pad 1) $
        renderCPUGraph s
    ]

legendBox :: Widget ResourceName
legendBox = hLimit 100 $ hCenter $
  borderWithLabel (str "Controls") $
    vBox
      [ str "q - Quit",
        str "s - Sort processes",
        str "← - Left",
        str "→ - Right",
        str "↑ - Up",
        str "↓ - Down"
      ]

renderGraph :: GraphData -> Widget ResourceName
renderGraph graphData =
  let thinBars = map (`renderThinBar` 30) (V.toList (points graphData))
   in hBox $
          map (padLeft (Pad 0) . padRight (Pad 0)) thinBars

renderMemoryGraph :: UIState -> Widget ResourceName
renderMemoryGraph s = case memGraphData s of
  Just graphData -> borderWithLabel (str "Memory Usage") $ hLimit 100 $ hCenter $ renderGraph graphData
  Nothing -> hLimit 100 $ hCenter $ borderWithLabel (str "Memory Graph") $
    vBox [str "No memory data available"]

renderCPUGraph :: UIState -> Widget ResourceName
renderCPUGraph s = case cpuGraphData s of
  Just graphData -> borderWithLabel (str "CPU Usage") $ hLimit 100 $ hCenter $ renderGraph graphData
  Nothing -> hLimit 100 $ hCenter $ borderWithLabel (str "CPU Graph") $
    vBox [str "No CPU data available"]

renderProcesses :: UIState -> Widget ResourceName
renderProcesses s = tableWidget headers rows
  where
    headers =
      map
        addSortMarker
        [ ("(p)PID", SortPID),
          ("(o)Command", SortCommand),
          ("(c)CPU %", SortCPU),
          ("(m)Memory %", SortMem),
          ("(v)VSZ", SortVSZ),
          ("(r)RSS", SortRSS),
          ("(t)TTY", SortTTY),
          ("(a)STAT", SortStat),
          ("(b)Started", SortStarted),
          ("(i)Time", SortTime),
          ("(u)User", SortUser)
        ]
    addSortMarker (header, col) = if col == tableSort s then "*" ++ header else header
    rows = map makeRow (sortProcessList (tableSort s) (processStats $ systemState s))


makeRow :: Process -> [String]
makeRow p =
  [ show (pid p),
    last (splitOn "/" (cmd p)),
    show (cpuPct p),
    show (memPct p),
    show (vsz p),
    show (rss p),
    tt p,
    stat p,
    started p,
    time p,
    user p
  ]

sortProcessList :: SortColumn -> ProcessList -> ProcessList
sortProcessList sortCol = case sortCol of
  SortPID -> sortBy (compare `on` pid)
  SortCommand -> sortBy (compare `on` cmd)
  SortCPU -> sortBy (flip compare `on` (read . show . cpuPct :: Process -> Double))
  SortMem -> sortBy (flip compare `on` (read . show . memPct :: Process -> Double))
  SortVSZ -> sortBy (flip compare `on` vsz)
  SortRSS -> sortBy (flip compare `on` rss)
  SortTTY -> sortBy (compare `on` tt)
  SortStat -> sortBy (compare `on` stat)
  SortStarted -> sortBy (compare `on` started)
  SortTime -> sortBy (compare `on` time)
  SortUser -> sortBy (compare `on` user)

handleEvent :: BrickEvent ResourceName CustomEvent -> EventM ResourceName UIState ()
handleEvent e = case e of
  VtyEvent vtye ->
    case vtye of
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey (V.KChar 's') [] ->
        modify $ \s -> s { awaitingKey = True }
      V.EvKey KLeft [] -> modify (\s -> s {orientation = RightLeft})
      V.EvKey KRight [] -> modify (\s -> s {orientation = LeftRight})
      V.EvKey KUp [] -> modify (\s -> s {orientation = TopBottom})
      V.EvKey KDown [] -> modify (\s -> s {orientation = BottomTop})
      V.EvKey (V.KChar c) [] ->
        whenAwaitingKey c
      _ -> return ()
  AppEvent Tick ->
    updateSystemState
  _ -> return ()

whenAwaitingKey :: Char -> EventM ResourceName UIState ()
whenAwaitingKey c = do
  s <- get
  when (awaitingKey s) $ do
    -- liftIO $ putStrLn $ "You pressed: " ++ [c]
    case c of
      'p' -> setSortColumn SortPID
      'o' -> setSortColumn SortCommand
      'c' -> setSortColumn SortCPU
      'm' -> setSortColumn SortMem
      'v' -> setSortColumn SortVSZ
      'r' -> setSortColumn SortRSS
      't' -> setSortColumn SortTTY
      'a' -> setSortColumn SortStat
      'b' -> setSortColumn SortStarted
      'i' -> setSortColumn SortTime
      'u' -> setSortColumn SortUser
      _   -> return ()
    modify $ \s' -> s' { awaitingKey = False }

setSortColumn :: SortColumn -> EventM ResourceName UIState ()
setSortColumn col = modify $ \s -> s { tableSort = col }

-- updateSystemState :: EventM ResourceName UIState ()
-- updateSystemState = do
--   mNewState <- liftIO gatherSystemState
--   case mNewState of
--     Just newSysState ->
--       modify $ \s -> s { systemState = newSysState }
--     Nothing -> return ()
updateSystemState :: EventM ResourceName UIState ()
updateSystemState = do
  mNewState <- liftIO gatherSystemState
  case mNewState of
    Just newSysState -> do
      modify $ \s ->
        let cpuUsage = calculateCPUPercentage (cpuStats newSysState)
            memUsage = calculateMemoryPercentage (memoryStats newSysState)
         in s
              { systemState = newSysState,
                cpuGraphData = Just $ addDataPoint cpuUsage (getOrInitGraph (cpuGraphData s)),
                memGraphData = Just $ addDataPoint memUsage (getOrInitGraph (memGraphData s))
              }
    Nothing -> return ()

calculateCPUPercentage :: ProcessedCPU -> Double
calculateCPUPercentage = totalUsage

calculateMemoryPercentage :: ProcessedMemory -> Double
calculateMemoryPercentage memStats =
  let total = fromIntegral (totalMem memStats)
      used = fromIntegral (usedMem memStats)
   in if total > 0 then (used / total) * 100 else 0

getOrInitGraph :: Maybe GraphData -> GraphData
getOrInitGraph (Just graph) = graph
getOrInitGraph Nothing = GraphData
  { points = V.replicate 600 0,
    maxPoints = 600
  }

addDataPoint :: Double -> GraphData -> GraphData
addDataPoint newPoint graph =
  graph { points = V.take (maxPoints graph) (V.cons newPoint (points graph)) }


-- renderGraph :: GraphData -> Widget ResourceName
-- renderGraph graphData =
--   let thinBars = map (`renderThinBar` 30) (V.toList (points graphData))
--    in borderWithLabel (str $ "CPU Usage - PID " ++ show (processId graphData)) $
--         hBox $
--           map (padLeft (Pad 0) . padRight (Pad 0)) thinBars

-- handleEvent :: BrickEvent ResourceName CustomEvent -> EventM ResourceName UIState ()
-- handleEvent e = case e of
--   VtyEvent vtye ->
--     case vtye of
--       V.EvKey (V.KChar 'q') [] -> halt
--       V.EvKey (V.KChar 's') [] ->
--         modify $ \s -> s {awaitingKey = True}
--       V.EvKey (V.KChar c) [] -> do
--         s <- get
--         when (awaitingKey s) $ do
--           liftIO $ putStrLn $ "You pressed: " ++ [c]
--           case c of
--             'p' -> modify $ \s' -> s' {tableSort = SortPID}
--             'o' -> modify $ \s' -> s' {tableSort = SortCommand}
--             'c' -> modify $ \s' -> s' {tableSort = SortCPU}
--             'm' -> modify $ \s' -> s' {tableSort = SortMem}
--             'v' -> modify $ \s' -> s' {tableSort = SortVSZ}
--             'r' -> modify $ \s' -> s' {tableSort = SortRSS}
--             't' -> modify $ \s' -> s' {tableSort = SortTTY}
--             'a' -> modify $ \s' -> s' {tableSort = SortStat}
--             'b' -> modify $ \s' -> s' {tableSort = SortStarted}
--             'i' -> modify $ \s' -> s' {tableSort = SortTime}
--             'u' -> modify $ \s' -> s' {tableSort = SortUser}
--             _ -> return ()
--           modify $ \s' -> s' {awaitingKey = False}
--       _ -> return ()
--   AppEvent Tick -> do
--     mNewState <- liftIO gatherSystemState
--     case mNewState of
--       Just newSysState ->
--         modify $ \s -> s {systemState = newSysState}
--       Nothing ->
--         return ()
--   _ -> return ()
