{-# LANGUAGE MultiWayIf #-}
-- | This module provides a table widget that can draw other widgets
-- in a table layout, draw borders between rows and columns, and allow
-- configuration of row and column alignment. To get started, see
-- 'table'.
module Brick.Widgets.Table
  (
  -- * Types
    Table
  , ColumnAlignment(..)
  , RowAlignment(..)
  , TableException(..)

  -- * Construction
  , table

  -- * Configuration
  , alignLeft
  , alignRight
  , alignCenter
  , alignTop
  , alignMiddle
  , alignBottom
  , setColAlignment
  , setRowAlignment
  , setDefaultColAlignment
  , setDefaultRowAlignment
  , surroundingBorder
  , rowBorders
  , columnBorders

  -- * Rendering
  , renderTable
  )
where

import Control.Monad (forM)
import qualified Control.Exception as E
import Data.List (transpose, intersperse, nub)
import qualified Data.Map as M
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Graphics.Vty (imageHeight, imageWidth, charFill)
import Lens.Micro ((^.))

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

-- | Column alignment modes. Use these modes with the alignment
-- functions in this module to configure column alignment behavior.
data ColumnAlignment =
    AlignLeft
    -- ^ Align all cells to the left.
    | AlignCenter
    -- ^ Center the content horizontally in all cells in the column.
    | AlignRight
    -- ^ Align all cells to the right.
    deriving (Eq, Show, Read)

-- | Row alignment modes. Use these modes with the alignment functions
-- in this module to configure row alignment behavior.
data RowAlignment =
    AlignTop
    -- ^ Align all cells to the top.
    | AlignMiddle
    -- ^ Center the content vertically in all cells in the row.
    | AlignBottom
    -- ^ Align all cells to the bottom.
    deriving (Eq, Show, Read)

-- | A table creation exception.
data TableException =
    TEUnequalRowSizes
    -- ^ Rows did not all have the same number of cells.
    | TEInvalidCellSizePolicy
    -- ^ Some cells in the table did not use the 'Fixed' size policy for
    -- both horizontal and vertical sizing.
    deriving (Eq, Show, Read)

instance E.Exception TableException where

-- | A table data structure for widgets of type 'Widget' @n@. Create a
-- table with 'table'.
data Table n =
    Table { columnAlignments :: M.Map Int ColumnAlignment
          , rowAlignments :: M.Map Int RowAlignment
          , tableRows :: [[Widget n]]
          , defaultColumnAlignment :: ColumnAlignment
          , defaultRowAlignment :: RowAlignment
          , drawSurroundingBorder :: Bool
          , drawRowBorders :: Bool
          , drawColumnBorders :: Bool
          }

-- | Construct a new table.
--
-- The argument is the list of rows, with each element of the argument
-- list being the contents of the cells in in each column of the
-- respective row.
--
-- By default:
--
-- * All columns are left-aligned. Use the alignment functions in this
-- module to change that behavior.
-- * All rows are top-aligned. Use the alignment functions in this
-- module to change that behavior.
-- * The table will draw borders between columns, between rows, and
-- around the outside of the table. Border-drawing behavior can be
-- configured with the API in this module. Note that tables always draw
-- with 'joinBorders' enabled. If a cell's contents has smart borders
-- but you don't want those borders to connect to the surrounding table
-- borders, wrap the cell's contents with 'freezeBorders'.
--
-- All cells of all rows MUST use the 'Fixed' growth policy for both
-- horizontal and vertical growth. If the argument list contains any
-- cells that use the 'Greedy' policy, this function will raise a
-- 'TableException'.
--
-- All rows MUST have the same number of cells. If not, this function
-- will raise a 'TableException'.
table :: [[Widget n]] -> Table n
table rows =
    if | not allFixed      -> E.throw TEInvalidCellSizePolicy
       | not allSameLength -> E.throw TEUnequalRowSizes
       | otherwise         -> t
    where
        allSameLength = length (nub (length <$> rows)) <= 1
        allFixed = all fixedRow rows
        fixedRow = all fixedCell
        fixedCell w = hSize w == Fixed && vSize w == Fixed
        t = Table { columnAlignments = mempty
                  , rowAlignments = mempty
                  , tableRows = rows
                  , drawSurroundingBorder = True
                  , drawRowBorders = True
                  , drawColumnBorders = True
                  , defaultColumnAlignment = AlignLeft
                  , defaultRowAlignment = AlignTop
                  }

-- | Configure whether the table draws a border on its exterior.
surroundingBorder :: Bool -> Table n -> Table n
surroundingBorder b t =
    t { drawSurroundingBorder = b }

-- | Configure whether the table draws borders between its rows.
rowBorders :: Bool -> Table n -> Table n
rowBorders b t =
    t { drawRowBorders = b }

-- | Configure whether the table draws borders between its columns.
columnBorders :: Bool -> Table n -> Table n
columnBorders b t =
    t { drawColumnBorders = b }

-- | Align the specified column to the right. The argument is the column
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignRight :: Int -> Table n -> Table n
alignRight = setColAlignment AlignRight

-- | Align the specified column to the left. The argument is the column
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignLeft :: Int -> Table n -> Table n
alignLeft = setColAlignment AlignLeft

-- | Align the specified column to center. The argument is the column
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignCenter :: Int -> Table n -> Table n
alignCenter = setColAlignment AlignCenter

-- | Align the specified row to the top. The argument is the row index,
-- starting with zero. Silently does nothing if the index is out of
-- range.
alignTop :: Int -> Table n -> Table n
alignTop = setRowAlignment AlignTop

-- | Align the specified row to the middle. The argument is the row
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignMiddle :: Int -> Table n -> Table n
alignMiddle = setRowAlignment AlignMiddle

-- | Align the specified row to bottom. The argument is the row index,
-- starting with zero. Silently does nothing if the index is out of
-- range.
alignBottom :: Int -> Table n -> Table n
alignBottom = setRowAlignment AlignBottom

-- | Set the alignment for the specified column index (starting at
-- zero). Silently does nothing if the index is out of range.
setColAlignment :: ColumnAlignment -> Int -> Table n -> Table n
setColAlignment a col t =
    t { columnAlignments = M.insert col a (columnAlignments t) }

-- | Set the alignment for the specified row index (starting at
-- zero). Silently does nothing if the index is out of range.
setRowAlignment :: RowAlignment -> Int -> Table n -> Table n
setRowAlignment a row t =
    t { rowAlignments = M.insert row a (rowAlignments t) }

-- | Set the default column alignment for columns with no explicitly
-- configured alignment.
setDefaultColAlignment :: ColumnAlignment -> Table n -> Table n
setDefaultColAlignment a t =
    t { defaultColumnAlignment = a }

-- | Set the default row alignment for rows with no explicitly
-- configured alignment.
setDefaultRowAlignment :: RowAlignment -> Table n -> Table n
setDefaultRowAlignment a t =
    t { defaultRowAlignment = a }

-- | Render the table.
renderTable :: Table n -> Widget n
renderTable t =
    joinBorders $
    Widget Fixed Fixed $ do
        ctx <- getContext
        let rows = tableRows t
        cellResults <- forM rows $ mapM render
        let maybeIntersperse f v = if f t then intersperse v else id
            rowHeights = rowHeight <$> cellResults
            colWidths = colWidth <$> byColumn
            allRowAligns = (\i -> M.findWithDefault (defaultRowAlignment t) i (rowAlignments t)) <$>
                           [0..length rowHeights - 1]
            allColAligns = (\i -> M.findWithDefault (defaultColumnAlignment t) i (columnAlignments t)) <$>
                           [0..length byColumn - 1]
            rowHeight = maximum . fmap (imageHeight . image)
            colWidth = maximum . fmap (imageWidth . image)
            byColumn = transpose cellResults
            toW = Widget Fixed Fixed . return
            applyColAlignment align width w =
                Widget Fixed Fixed $ do
                    result <- render w
                    case align of
                        AlignLeft -> render $ hLimit width $ padRight Max $ toW result
                        AlignCenter -> render $ hLimit width $ hCenter $ toW result
                        AlignRight -> render $
                                          padLeft (Pad (width - imageWidth (image result))) $
                                          toW result
            applyRowAlignment rHeight align result =
                case align of
                 AlignTop -> vLimit rHeight $ padBottom Max $ toW result
                 AlignMiddle -> vLimit rHeight $ vCenter $ toW result
                 AlignBottom -> vLimit rHeight $ padTop Max $ toW result
            fixEmtpyCell w h result =
                if imageWidth (image result) == 0 && imageHeight (image result) == 0
                then result { image = charFill (ctx^.attrL) ' ' w h }
                else result
            mkColumn (hAlign, width, colCells) = do
                let paddedCells = flip map (zip3 allRowAligns rowHeights colCells) $ \(vAlign, rHeight, cell) ->
                        applyColAlignment hAlign width $
                        applyRowAlignment rHeight vAlign $
                        fixEmtpyCell width rHeight cell
                    maybeRowBorders = maybeIntersperse drawRowBorders (hLimit width hBorder)
                render $ vBox $ maybeRowBorders paddedCells
        columns <- mapM mkColumn $ zip3 allColAligns colWidths byColumn

        let tl = joinableBorder (Edges False True False True)
            tr = joinableBorder (Edges False True True False)
            bl = joinableBorder (Edges True False False True)
            br = joinableBorder (Edges True False True False)
            cross = joinableBorder (Edges True True True True)
            leftT = joinableBorder (Edges True True False True)
            rightT = joinableBorder (Edges True True True False)
            topT = joinableBorder (Edges False True True True)
            bottomT = joinableBorder (Edges True False True True)
            vBorders = mkVBorder <$> rowHeights
            hBorders = mkHBorder <$> colWidths
            mkHBorder w = hLimit w hBorder
            mkVBorder h = vLimit h vBorder
            topBorder = hBox $ maybeIntersperse drawColumnBorders topT hBorders
            bottomBorder = hBox $ maybeIntersperse drawColumnBorders bottomT hBorders
            leftBorder = vBox $ tl : maybeIntersperse drawRowBorders leftT vBorders <> [bl]
            rightBorder = vBox $ tr : maybeIntersperse drawRowBorders rightT vBorders <> [br]
            maybeAddSurroundingBorder body =
                if not $ drawSurroundingBorder t
                then body
                else leftBorder <+> (topBorder <=> body <=> bottomBorder) <+> rightBorder
            maybeAddColumnBorders =
                if not $ drawColumnBorders t
                then id
                else let maybeAddCrosses = maybeIntersperse drawRowBorders cross
                         columnBorder = vBox $ maybeAddCrosses vBorders
                     in intersperse columnBorder

        render $ maybeAddSurroundingBorder $
                 hBox $
                 maybeAddColumnBorders $ toW <$> columns
