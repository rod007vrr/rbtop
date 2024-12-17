module UI.Graph where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core (str, padRight, padLeft, vBox, hBox)
import qualified Data.Vector.Unboxed as V
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Data structure to hold graph data
data GraphData = GraphData
  { points :: V.Vector Double  -- List of CPU percentages
  , maxPoints :: Int           -- Max number of points (window size)
  , processId :: Int           -- Process ID this graph belongs to
  } deriving (Show, Eq)

-- Initialize a new graph with random data
initializeGraph :: Int -> IO GraphData
initializeGraph pid = do
  initialPoints <- replicateM 600 (randomRIO (0, 100))  -- Generate 600 points
  return GraphData
    { points = V.fromList initialPoints
    , maxPoints = 600
    , processId = pid
    }

-- Add a new data point and maintain the maxPoints window
addDataPoint :: Double -> GraphData -> GraphData
addDataPoint newPoint graph =
  graph { points = V.take (maxPoints graph) (V.cons newPoint (points graph)) }


renderThinBar :: Double -> Int -> Widget n
renderThinBar value maxHeight =
  let scaledHeight = round (value / 100 * fromIntegral maxHeight)
      barSegments = replicate scaledHeight '⣿'                  
      barWidget = vBox (map (str . (: [])) barSegments)          
  in padTop (Pad (maxHeight - scaledHeight)) barWidget            