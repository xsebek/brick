{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Main where

import Control.Monad (void)
import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( str
  )
import qualified Brick.AttrMap as A

data Name1
    deriving (Ord, Show, Eq)

data Name2
    deriving (Ord, Show, Eq)

data St1 =
    St1 { _num1 :: Int
        , _num2_old :: Int
        }

data St2 =
    St2 { _num2 :: Int
        }

makeLenses ''St1
makeLenses ''St2

drawApp1 :: St1 -> [T.Widget Name1]
drawApp1 st = [str $ "This is application 1. Press 't' to transition to " <>
                     "app 2, or +/- to change num1.\nnum1 = " <> show (st^.num1) <>
                     "\nnum2 = " <> show (st^.num2_old)]

drawApp2 :: St2 -> [T.Widget Name2]
drawApp2 st = [str $ "This is application 2. Press +/- to change num2.\n" <>
                     "Press Esc to return to application 1.\n" <>
                     "num2 = " <> show (st^.num2)]

app1Event :: St1 -> T.BrickEvent Name1 e -> T.EventM Name1 (T.Next St1)
app1Event st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] ->
            M.halt st
        V.EvKey (V.KChar 't') [] ->
            M.switchApp (St2 0) app2 Nothing (\st2 -> st { _num2_old = st2^.num2 })
        V.EvKey (V.KChar '+') [] ->
            M.continue $ st & num1 %~ succ
        V.EvKey (V.KChar '-') [] ->
            M.continue $ st & num1 %~ pred
        _ ->
            M.continue st
app1Event st _ = M.continue st

app2Event :: St2 -> T.BrickEvent Name2 e -> T.EventM Name2 (T.Next St2)
app2Event st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] ->
            M.halt st
        V.EvKey (V.KChar '+') [] ->
            M.continue $ st & num2 %~ succ
        V.EvKey (V.KChar '-') [] ->
            M.continue $ st & num2 %~ pred
        _ ->
            M.continue st
app2Event st _ = M.continue st

app1 :: M.App St1 e Name1
app1 =
    M.App { M.appDraw = drawApp1
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = app1Event
          , M.appStartEvent = return
          , M.appAttrMap = const $ A.attrMap V.defAttr []
          }

app2 :: M.App St2 e Name2
app2 =
    M.App { M.appDraw = drawApp2
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = app2Event
          , M.appStartEvent = return
          , M.appAttrMap = const $ A.attrMap V.defAttr []
          }

main :: IO ()
main = do
    void $ M.defaultMain app1 $ St1 0 0
