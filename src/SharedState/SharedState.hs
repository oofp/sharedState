{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module SharedState
  ( MutableWrapper
  ) where

import Prelude

import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM (atomically, readTVar,writeTVar)
import Control.Monad.Reader.Class (MonadReader,ask)
import Control.Monad.State.Class (MonadState, state)
import Control.Monad.IO.Class (MonadIO, liftIO)

class (Monad m) => MutableWrapper w m where
  update :: (s -> (a , s)) -> w s -> m a


instance (MutableWrapper w m, MonadReader (w  s) m, Monad m) => MonadState s m where
  --   state :: (s -> (a, s)) -> m a
  state f = do
    w_s <- ask
    update f w_s

instance (MonadIO m) => MutableWrapper TVar m where
  update f ts =
    liftIO $ atomically $ do
      s <- readTVar ts
      let (a, s1) = f s
      writeTVar ts s1
      return a
