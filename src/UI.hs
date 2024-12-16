{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module UI where 

import Brick.Main
import Brick.Types
import Brick.AttrMap
import Graphics.Vty
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center
import CPUCollector

ui :: IO ()
ui = do 
    initialState <- buildInitialState
    endState <- defaultMain (app :: App UIState e ResourceName) initialState
    print endState

buildInitialState :: IO UIState 
buildInitialState = do 
    pure UIState {cpuData = mockCPUData}

mockCPUData :: ProcessedCPU
mockCPUData = ProcessedCPU 
    { timestamp = 1234567890
    , totalUsage = 75.5
    , userUsage = 45.2
    , systemUsage = 30.3
    , idleUsage = 24.5
    , ioWaitUsage = 10.2
    }

type ResourceName = String

data UIState = UIState
    { cpuData :: ProcessedCPU } deriving (Show, Eq)

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
    [ withAttr headerAttr $ hCenter $ str "CPU Usage Statistics"
    , hCenter $ borderBox $ vBox
        [ hBox [ padRight (Pad 2) $ str "Total CPU:"
               , str $ show (totalUsage $ cpuData s) ++ "%"]
        , hBox [ padRight (Pad 2) $ str "User Space:"
               , str $ show (userUsage $ cpuData s) ++ "%"]
        , hBox [ padRight (Pad 2) $ str "System:"
               , str $ show (systemUsage $ cpuData s) ++ "%"]
        , hBox [ padRight (Pad 2) $ str "Idle:"
               , str $ show (idleUsage $ cpuData s) ++ "%"]
        , hBox [ padRight (Pad 2) $ str "IO Wait:"
               , str $ show (ioWaitUsage $ cpuData s) ++ "%"]
        ]
    ]]
    where
        borderBox = border . padLeft (Pad 1) . padRight (Pad 1)

handleEvent :: BrickEvent n e -> EventM n UIState ()
handleEvent e = case e of 
    VtyEvent vtye -> 
        case vtye of
            EvKey (KChar 'q') [] -> halt
            _ -> return ()
    _ -> return ()

