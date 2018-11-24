{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module Brick.Types.Internal
  ( ScrollRequest(..)
  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL
  , Location(..)
  , locL
  , origin
  , TerminalLocation(..)
  , App(..)
  , Widget(..)
  , EventM(..)
  , RenderM
  , Size(..)
  , Viewport(..)
  , ViewportType(..)
  , RenderState(..)
  , Direction(..)
  , CursorLocation(..)
  , cursorLocationL
  , cursorLocationNameL
  , Context(..)
  , EventState(..)
  , EventRO(..)
  , Next(..)
  , Result(..)
  , Extent(..)
  , Edges(..)
  , eTopL, eBottomL, eRightL, eLeftL
  , BorderSegment(..)
  , bsAcceptL, bsOfferL, bsDrawL
  , DynBorder(..)
  , dbStyleL, dbAttrL, dbSegmentsL
  , CacheInvalidateRequest(..)
  , BrickEvent(..)

  , rsScrollRequestsL
  , viewportMapL
  , clickableNamesL
  , renderCacheL
  , observedNamesL
  , vpSize
  , vpLeft
  , vpTop
  , imageL
  , cursorsL
  , extentsL
  , bordersL
  , visibilityRequestsL
  , emptyResult
  )
where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Lens.Micro (_1, _2, Lens')
import Lens.Micro.TH (makeLenses)
import qualified Data.Set as S
import qualified Data.Map as M
import Graphics.Vty (Vty, Event, Button, Modifier, DisplayRegion, Image, Attr, emptyImage)
import GHC.Generics
import Control.DeepSeq (NFData)

import Brick.BChan (BChan)
import Brick.BorderMap (BorderMap)
import qualified Brick.BorderMap as BM
import Brick.Types.Common
import Brick.Types.TH
import Brick.AttrMap (AttrName, AttrMap)
import Brick.Widgets.Border.Style (BorderStyle)

data ScrollRequest = HScrollBy Int
                   | HScrollPage Direction
                   | HScrollToBeginning
                   | HScrollToEnd
                   | VScrollBy Int
                   | VScrollPage Direction
                   | VScrollToBeginning
                   | VScrollToEnd
                   | SetTop Int
                   | SetLeft Int
                   deriving (Read, Show, Generic, NFData)

data VisibilityRequest =
    VR { vrPosition :: Location
       , vrSize :: DisplayRegion
       }
       deriving (Show, Eq, Read, Generic, NFData)

-- | Describes the state of a viewport as it appears as its most recent
-- rendering.
data Viewport =
    VP { _vpLeft :: Int
       -- ^ The column offset of left side of the viewport.
       , _vpTop :: Int
       -- ^ The row offset of the top of the viewport.
       , _vpSize :: DisplayRegion
       -- ^ The size of the viewport.
       }
       deriving (Show, Read, Generic, NFData)

-- | The type of viewports that indicates the direction(s) in which a
-- viewport is scrollable.
data ViewportType = Vertical
                  -- ^ Viewports of this type are scrollable only vertically.
                  | Horizontal
                  -- ^ Viewports of this type are scrollable only horizontally.
                  | Both
                  -- ^ Viewports of this type are scrollable vertically and horizontally.
                  deriving (Show, Eq)

data CacheInvalidateRequest n =
    InvalidateSingle n
    | InvalidateEntire
    deriving (Ord, Eq)

data EventState n = ES { esScrollRequests :: [(n, ScrollRequest)]
                       , cacheInvalidateRequests :: S.Set (CacheInvalidateRequest n)
                       }

-- | An extent of a named area: its size, location, and origin.
data Extent n = Extent { extentName      :: n
                       , extentUpperLeft :: Location
                       , extentSize      :: (Int, Int)
                       , extentOffset    :: Location
                       }
                       deriving (Show, Read, Generic, NFData)

-- | Scrolling direction.
data Direction = Up
               -- ^ Up/left
               | Down
               -- ^ Down/right
               deriving (Show, Eq, Read, Generic, NFData)

-- | The class of types that behave like terminal locations.
class TerminalLocation a where
    -- | Get the column out of the value
    locationColumnL :: Lens' a Int
    locationColumn :: a -> Int

    -- | Get the row out of the value
    locationRowL :: Lens' a Int
    locationRow :: a -> Int

instance TerminalLocation Location where
    locationColumnL = _1
    locationColumn (Location t) = fst t
    locationRowL = _2
    locationRow (Location t) = snd t

-- | A cursor location.  These are returned by the rendering process.
data CursorLocation n =
    CursorLocation { cursorLocation :: !Location
                   -- ^ The location
                   , cursorLocationName :: !(Maybe n)
                   -- ^ The name of the widget associated with the location
                   }
                   deriving (Read, Show, Generic, NFData)

-- | A border character has four segments, one extending in each direction
-- (horizontally and vertically) from the center of the character.
data BorderSegment = BorderSegment
    { bsAccept :: Bool
    -- ^ Would this segment be willing to be drawn if a neighbor wanted to
    -- connect to it?
    , bsOffer :: Bool
    -- ^ Does this segment want to connect to its neighbor?
    , bsDraw :: Bool
    -- ^ Should this segment be represented visually?
    } deriving (Eq, Ord, Read, Show, Generic, NFData)

suffixLenses ''BorderSegment

-- | Information about how to redraw a dynamic border character when it abuts
-- another dynamic border character.
data DynBorder = DynBorder
    { dbStyle :: BorderStyle
    -- ^ The 'Char's to use when redrawing the border. Also used to filter
    -- connections: only dynamic borders with equal 'BorderStyle's will connect
    -- to each other.
    , dbAttr :: Attr
    -- ^ What 'Attr' to use to redraw the border character. Also used to filter
    -- connections: only dynamic borders with equal 'Attr's will connect to
    -- each other.
    , dbSegments :: Edges BorderSegment
    } deriving (Eq, Read, Show, Generic, NFData)

suffixLenses ''DynBorder

-- | The type of result returned by a widget's rendering function. The
-- result provides the image, cursor positions, and visibility requests
-- that resulted from the rendering process.
data Result n =
    Result { image :: Image
           -- ^ The final rendered image for a widget
           , cursors :: [CursorLocation n]
           -- ^ The list of reported cursor positions for the
           -- application to choose from
           , visibilityRequests :: [VisibilityRequest]
           -- ^ The list of visibility requests made by widgets rendered
           -- while rendering this one (used by viewports)
           , extents :: [Extent n]
           -- Programmer's note: we don't try to maintain the invariant that
           -- the size of the borders closely matches the size of the 'image'
           -- field. Most widgets don't need to care about borders, and so they
           -- use the empty 'BorderMap' that has a degenerate rectangle. Only
           -- border-drawing widgets and the hbox/vbox stuff try to set this
           -- carefully. Even then, in the boxes, we only make sure that the
           -- 'BorderMap' is no larger than the entire concatenation of boxes,
           -- and it's certainly possible for it to be smaller. (Resizing
           -- 'BorderMap's is lossy, so we try to do it as little as possible.)
           -- If you're writing a widget, this should make it easier for you to
           -- do so; but beware this lack of invariant if you are consuming
           -- widgets.
           , borders :: BorderMap DynBorder
           -- ^ Places where we may rewrite the edge of the image when
           -- placing this widget next to another one.
           }
           deriving (Show, Read, Generic, NFData)

suffixLenses ''Result

emptyResult :: Result n
emptyResult = Result emptyImage [] [] [] BM.empty

-- | Widget growth policies. These policies communicate to layout
-- algorithms how a widget uses space when being rendered. These
-- policies influence rendering order and space allocation in the box
-- layout algorithm.
data Size = Fixed
          -- ^ Fixed widgets take up the same amount of space no matter
          -- how much they are given (non-greedy).
          | Greedy
          -- ^ Greedy widgets take up all the space they are given.
          deriving (Show, Eq, Ord)

-- | The type of widgets.
data Widget n =
    Widget { hSize :: Size
           -- ^ This widget's horizontal growth policy
           , vSize :: Size
           -- ^ This widget's vertical growth policy
           , render :: RenderM n (Result n)
           -- ^ This widget's rendering function
           }

-- | The type of the rendering monad. This monad is used by the
-- library's rendering routines to manage rendering state and
-- communicate rendering parameters to widgets' rendering functions.
type RenderM n a = ReaderT Context (State (RenderState n)) a

-- | The monad in which event handlers run. Although it may be tempting
-- to dig into the reader value yourself, just use
-- 'Brick.Main.lookupViewport'.
newtype EventM n a =
    EventM { runEventM :: ReaderT (EventRO n) (StateT (EventState n) IO) a
           }
           deriving (Functor) -- , Applicative, Monad, MonadIO)

instance Monad (EventM n) where
    a >>= f = EventM $ do
        val <- runEventM a
        runEventM $ f val
    return = EventM . return

instance Applicative (EventM n) where
    pure = return
    f <*> a = EventM $ do
        func <- runEventM f
        v <- runEventM a
        return $ func v

instance MonadIO (EventM n) where
    liftIO = EventM . liftIO

-- | The library application abstraction. Your application's operations
-- are represented here and passed to one of the various main functions
-- in this module. An application is in terms of an application state
-- type 's', an application event type 'e', and a resource name type
-- 'n'. In the simplest case 'e' is unused (left polymorphic or set to
-- '()'), but you may define your own event type and use 'customMain'
-- to provide custom events. The state type is the type of application
-- state to be provided by you and iteratively modified by event
-- handlers. The resource name type is the type of names you can assign
-- to rendering resources such as viewports and cursor locations.
data App s e n =
    App { appDraw :: s -> [Widget n]
        -- ^ This function turns your application state into a list of
        -- widget layers. The layers are listed topmost first.
        , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
        -- ^ This function chooses which of the zero or more cursor
        -- locations reported by the rendering process should be
        -- selected as the one to use to place the cursor. If this
        -- returns 'Nothing', no cursor is placed. The rationale here
        -- is that many widgets may request a cursor placement but your
        -- application state is what you probably want to use to decide
        -- which one wins.
        , appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
        -- ^ This function takes the current application state and an
        -- event and returns an action to be taken and a corresponding
        -- transformed application state. Possible options are
        -- 'continue', 'suspendAndResume', and 'halt'.
        , appStartEvent :: s -> EventM n s
        -- ^ This function gets called once just prior to the first
        -- drawing of your application. Here is where you can make
        -- initial scrolling requests, for example.
        , appAttrMap :: s -> AttrMap
        -- ^ The attribute map that should be used during rendering.
        }

-- | The type of actions to take upon completion of an event handler.
data Next a where
    Continue :: a -> Next a
    SuspendAndResume :: IO a -> Next a
    Halt :: a -> Next a
    Transition :: (Ord n) => b -> App b e n -> Maybe (BChan e) -> (b -> a) -> Next a

-- | The type of events.
data BrickEvent n e = VtyEvent Event
                    -- ^ The event was a Vty event.
                    | AppEvent e
                    -- ^ The event was an application event.
                    | MouseDown n Button [Modifier] Location
                    -- ^ A mouse-down event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    | MouseUp n (Maybe Button) Location
                    -- ^ A mouse-up event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    deriving (Show, Eq, Ord)

data RenderState n =
    RS { viewportMap :: M.Map n Viewport
       , rsScrollRequests :: [(n, ScrollRequest)]
       , observedNames :: !(S.Set n)
       , renderCache :: M.Map n (Result n)
       , clickableNames :: [n]
       } deriving (Read, Show, Generic, NFData)

data EventRO n = EventRO { eventViewportMap :: M.Map n Viewport
                         , eventVtyHandle :: Vty
                         , latestExtents :: [Extent n]
                         , oldState :: RenderState n
                         }

-- | The rendering context. This tells widgets how to render: how much
-- space they have in which to render, which attribute they should use
-- to render, which bordering style should be used, and the attribute map
-- available for rendering.
data Context =
    Context { ctxAttrName :: AttrName
            , availWidth :: Int
            , availHeight :: Int
            , ctxBorderStyle :: BorderStyle
            , ctxAttrMap :: AttrMap
            , ctxDynBorders :: Bool
            }
            deriving Show

suffixLenses ''RenderState
suffixLenses ''VisibilityRequest
suffixLenses ''CursorLocation
makeLenses ''Viewport
