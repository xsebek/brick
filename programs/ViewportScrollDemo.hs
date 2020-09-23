{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  , withHScrollBarPolicy
  , withVScrollBarPolicy
  , scrollBarAttr
  )
import Brick.Util (bg)

data Name = VP1
          | VP2
          | VP3
          deriving (Ord, Show, Eq)

drawUi :: T.ScrollBarPolicy -> [Widget Name]
drawUi p = [withHScrollBarPolicy p $ withVScrollBarPolicy p ui]
    where
        ui = C.center $
             vBox [ B.border $ hLimit 60 $ vLimit 21 $
                    vBox [ pair, B.hBorder, singleton ]
                  , hLimit 60 $ C.hCenter $ str "Press Space to toggle scroll bars."
                  ]
        singleton = viewport VP3 Both $
                    vBox $ str "Press ctrl-arrow keys to scroll this viewport horizontally and vertically."
                         : (str <$> [ "Line " <> show i | i <- [2..25::Int] ])
        pair = hBox [ viewport VP1 Vertical $
                      vBox $ str "Press up and down arrow keys" :
                             str "to scroll this viewport." :
                             (str <$> [ "Line " <> (show i) | i <- [3..50::Int] ])
                    , B.vBorder
                    , viewport VP2 Horizontal $
                      str "Press left and right arrow keys to scroll this viewport."
                    ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

vp2Scroll :: M.ViewportScroll Name
vp2Scroll = M.viewportScroll VP2

vp3Scroll :: M.ViewportScroll Name
vp3Scroll = M.viewportScroll VP3

appEvent :: T.ScrollBarPolicy -> T.BrickEvent Name e -> T.EventM Name (T.Next T.ScrollBarPolicy)
appEvent p (T.VtyEvent (V.EvKey (V.KChar ' ') [])) =
    M.continue $ if p == T.Never then T.Always else T.Never
appEvent p (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) = M.vScrollBy vp3Scroll 1 >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KUp    [V.MCtrl])) = M.vScrollBy vp3Scroll (-1) >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = M.hScrollBy vp3Scroll 1 >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) = M.hScrollBy vp3Scroll (-1) >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1 >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1) >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KRight [])) = M.hScrollBy vp2Scroll 1 >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KLeft []))  = M.hScrollBy vp2Scroll (-1) >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt p
appEvent p _ = M.continue p

app :: M.App T.ScrollBarPolicy e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr [(scrollBarAttr, bg V.yellow)]
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app T.Never
