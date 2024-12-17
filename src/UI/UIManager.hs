{-# OPTIONS_GHC -Wno-unused-imports #-}

module UI.UIManager (ui) where

import Brick (customMain, get)
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
    str,
    vBox,
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Function
import Data.List (break, drop, sortBy)
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import Graphics.Vty (Event (EvKey), Key (KChar), blue, defAttr, eventChannel)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import ProcessCollector (Process (..), ProcessList)
import SystemState (SystemState (..), gatherSystemState)
import UI.Graph (GraphData (points, processId), renderThinBar)
import UI.Table (tableWidget)

headerAttr :: AttrName
headerAttr = attrName "header"

type ResourceName = String

data SortColumn
  = SortPID
  | SortCommand
  | SortCPU
  | SortMem
  | SortVSZ
  | SortRSS
  | SortTTY
  | SortStat
  | SortStarted
  | SortTime
  | SortUser
  deriving (Show, Eq)

data UIState = UIState
  { systemState :: SystemState,
    cpuGraphData :: Maybe GraphData,
    memGraphData :: Maybe GraphData,
    awaitingKey :: Bool,
    tableSort :: SortColumn
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
          tableSort = SortCPU
        }
    Nothing ->
      UIState
        { systemState = emptySystemState,
          cpuGraphData = Nothing,
          memGraphData = Nothing,
          awaitingKey = False,
          tableSort = SortCPU
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
drawUI s =
  [ vBox
      [ hBox
          [ padRight (Pad 2) $ renderTable s
          -- legendBox
          ]
          -- maybe emptyWidget renderGraph (cpuGraphData s)
      ]
  ]

-- legendBox :: Widget ResourceName
-- legendBox =
--   borderWithLabel (str "Controls") $
--     vBox
--       [ str "q - quit",
--         str "s - sort",
--         str "g - show graph"
--       ]

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

renderTable :: UIState -> Widget ResourceName
renderTable s = tableWidget headers rows
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

-- renderGraph :: GraphData -> Widget ResourceName
-- renderGraph graphData =
--   let thinBars = map (`renderThinBar` 30) (V.toList (points graphData))
--    in borderWithLabel (str $ "CPU Usage - PID " ++ show (processId graphData)) $
--         hBox $
--           map (padLeft (Pad 0) . padRight (Pad 0)) thinBars

handleEvent :: BrickEvent ResourceName CustomEvent -> EventM ResourceName UIState ()
handleEvent e = case e of
  VtyEvent vtye ->
    case vtye of
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey (V.KChar 's') [] ->
        modify $ \s -> s {awaitingKey = True}
      V.EvKey (V.KChar c) [] -> do
        s <- get
        when (awaitingKey s) $ do
          liftIO $ putStrLn $ "You pressed: " ++ [c]
          case c of
            'p' -> modify $ \s' -> s' {tableSort = SortPID}
            'o' -> modify $ \s' -> s' {tableSort = SortCommand}
            'c' -> modify $ \s' -> s' {tableSort = SortCPU}
            'm' -> modify $ \s' -> s' {tableSort = SortMem}
            'v' -> modify $ \s' -> s' {tableSort = SortVSZ}
            'r' -> modify $ \s' -> s' {tableSort = SortRSS}
            't' -> modify $ \s' -> s' {tableSort = SortTTY}
            'a' -> modify $ \s' -> s' {tableSort = SortStat}
            'b' -> modify $ \s' -> s' {tableSort = SortStarted}
            'i' -> modify $ \s' -> s' {tableSort = SortTime}
            'u' -> modify $ \s' -> s' {tableSort = SortUser}
            _ -> return ()
          modify $ \s' -> s' {awaitingKey = False}
      _ -> return ()
  AppEvent Tick -> do
    mNewState <- liftIO gatherSystemState
    case mNewState of
      Just newSysState ->
        modify $ \s -> s {systemState = newSysState}
      Nothing ->
        return ()
  _ -> return ()
