module UI.Table (tableWidget) where

import Brick
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.Center (hCenter)

type Header = [String]
type Row = [String]

-- | Render a table widget with headers and rows.
-- The table dynamically adjusts column widths and centers each cell's content.
tableWidget :: Header -> [Row] -> Widget n
tableWidget headers rows = border $ vBox (headerWidget : hBorder : map rowWidget rows)
  where
    -- Render a single row
    rowWidget :: Row -> Widget n
    rowWidget cells = hBox $ map (hCenter . str) cells

    -- Render the header
    headerWidget :: Widget n
    headerWidget = withAttr headerAttr $ rowWidget headers

-- Attribute for headers
headerAttr :: AttrName
headerAttr = attrName "header"
