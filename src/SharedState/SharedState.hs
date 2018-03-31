{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module SharedState
  ( MutableWrapper
  ) where

import Prelude

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class

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
