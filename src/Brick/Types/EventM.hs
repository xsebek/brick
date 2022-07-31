{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Brick.Types.EventM
  ( EventM(..)
  , getVtyContext
  )
where

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as SState
import qualified Control.Monad.State.Lazy as LState
import Lens.Micro.Mtl
import Lens.Micro.Mtl.Internal

import Brick.Types.Internal

-- | The monad in which event handlers run.
newtype EventM n s a =
    EventM { runEventM :: ReaderT (EventRO n) (LState.StateT s (SState.StateT (EventState n) IO)) a
           }
           deriving ( Functor, Applicative, Monad, MonadIO
                    , MonadThrow, MonadCatch, MonadMask, MonadFail
                    )

instance LState.MonadState s (EventM n s) where
    get = EventM $ lift LState.get
    put = EventM . lift . LState.put

getVtyContext :: EventM n s VtyContext
getVtyContext = EventM $ lift $ lift $ SState.gets vtyContext

type instance Zoomed (EventM n s) = Zoomed (LState.StateT s (SState.StateT (EventState n) IO))

instance Zoom (EventM n s) (EventM n t) s t where
    zoom l (EventM m) = EventM (zoom l m)
