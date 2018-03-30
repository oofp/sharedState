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

{-
newtype TVarState s a = TVarIntReader (ReaderT (TVar s) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (TVar s), MonadIO)

addState :: Int -> (MonadState Int m) => m Int
addState inc = state (\i -> (i+inc,i*inc))

newtype TVarIntReader a = TVarIntReader (ReaderT (TVar Int) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (TVar Int), MonadIO)

runTVarIntReader :: forall b. TVarIntReader b -> IO b
runTVarIntReader (TVarIntReader rd) = do
  zero <- newTVarIO (0::Int)
  runReaderT rd zero

adder2 :: TVarIntReader Int
adder2  = addState 2

adder2_10 :: TVarIntReader Int
adder2_10 = put 1 >> replicateM 10 adder2 >> get
-}
