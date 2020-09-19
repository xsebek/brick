{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
import Brick.Widgets.Core
import Brick.Util (bg, fg, on)

data Name =
    TheList
    deriving (Eq, Ord, Show)

drawUI :: (Show a) => L.List Name a -> [Widget Name]
drawUI l = [verticalScrollBarLayer TheList, ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total =  str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              padRight (Pad 1) $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

verticalScrollBarLayer :: (Ord n) => n -> Widget n
verticalScrollBarLayer n = Widget Fixed Fixed $ do
    mVp <- unsafeLookupViewport n
    mEx <- unsafeLookupExtent n
    render $ case (,) <$> mEx <*> mVp of
        Nothing -> emptyWidget
        Just (e, vp) ->
            translateBy off scrollbar
            where
                eOffset = extentUpperLeft e
                eRow = locationRow eOffset
                eCol = locationColumn eOffset
                col = eCol + (V.regionWidth $ _vpSize vp)
                off = Location (col, eRow)
                vpHeight = V.regionHeight $ _vpSize vp
                contentHeight = V.regionHeight $ _vpContentSize vp
                scrollbar =
                    -- If the content is smaller than the viewport,
                    -- render a full scrollbar
                    if vpHeight == 0 || contentHeight <= vpHeight
                    then withDefAttr scrollBarAttr $ vLimit vpHeight $ hLimit 1 $ fill ' '
                    else
                        -- Divide up the scroll bar into regions
                        -- indicating what is visible and what isn't
                        let ratio :: Float
                            ratio = fromIntegral contentHeight / fromIntegral vpHeight
                            (middleFixed, middle) =
                                let middle' = fromIntegral vpHeight / ratio
                                in if middle' > 0.0 && middle' < 1.0
                                   then (True, 1)
                                   else (False, truncate middle')
                            before =
                                let before' = ceiling $ (fromIntegral $ _vpTop vp) / ratio
                                in max 0 $ before' - (if middleFixed then 1 else 0)
                            after = max 0 $ vpHeight - (before + middle)
                        in padTop (Pad before) $ padBottom (Pad after) $
                           hLimit 1 $
                           vLimit middle $
                           withDefAttr scrollBarAttr $
                           fill ' '

scrollBarAttr :: A.AttrName
scrollBarAttr = "scrollbar"

appEvent :: L.List Name Char -> T.BrickEvent Name e -> T.EventM Name (T.Next (L.List Name Char))
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] ->
            let el = nextElement (L.listElements l)
                pos = Vec.length $ l^.(L.listElementsL)
            in M.continue $ L.listInsert pos el l

        V.EvKey (V.KChar '-') [] ->
            case l^.(L.listSelectedL) of
                Nothing -> M.continue l
                Just i -> M.continue $ L.listRemove i l

        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue =<< L.handleListEvent ev l
    where
      nextElement :: Vec.Vector Char -> Char
      nextElement v = fromMaybe '?' $ Vec.find (flip Vec.notElem v) (Vec.fromList ['a' .. 'z'])
appEvent l _ = M.continue l

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

initialState :: L.List Name Char
initialState = L.list TheList (Vec.fromList ['a','b','c']) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    , (scrollBarAttr,         bg V.yellow)
    ]

theApp :: M.App (L.List Name Char) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
