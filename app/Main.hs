{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude
import SharedState ()
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Reader.Class
import Control.Monad.State.Class (MonadState, state,put,get,modify)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad (replicateM,forever)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.Thread.Delay (delay)

newtype TVarMonadState s a = TVarMonadState (ReaderT (TVar s) IO a)
 deriving (Functor, Applicative, Monad, MonadReader (TVar s), MonadIO)

runSharedState :: TVarMonadState s a -> TVar s -> IO a
runSharedState (TVarMonadState rd)  = runReaderT rd

runTVarIntReader :: TVarMonadState s a -> s-> IO a
runTVarIntReader st s = do
  zero <- newTVarIO s
  runSharedState st zero

addState :: (MonadState Int m) => Int -> m String
addState inc = state (\i -> (let new_i= i+inc in (show new_i,new_i)))

adder2 :: TVarMonadState Int String
adder2  = addState 2

adder2_10 :: TVarMonadState Int Int
adder2_10 = put 1 >> replicateM 10 adder2 >> get

countSeconds :: TVarMonadState Int ()
countSeconds = forever $ do
  liftIO $ delay 1000000
  modify (+1)

promptLoop :: TVarMonadState Int Int
promptLoop = do -- runinside state monad
 liftIO $ putStrLn "Enter q to exit, ? -to get number of seconds, !-to reset counter, 2- to double,  and then [Enter]"
 inStr <- liftIO getLine
 case inStr of
   "q" -> get
   "?" -> do
           curCounter <- get
           liftIO (putStrLn ("Current counter is:" ++ show curCounter))
           promptLoop
   "!" ->  put 0  >>  promptLoop
   "2" ->  modify (*2)   >>  promptLoop
   [] ->   liftIO  (putStrLn ">") >> promptLoop
   _  ->   liftIO  (putStrLn "Wrong input")  >> promptLoop

twoThreads :: IO ()
twoThreads = do
   initState <- newTVarIO (0::Int)
   asyncTask <- async $ runSharedState countSeconds initState
   cnt <- runSharedState promptLoop initState
   cancel asyncTask
   putStrLn ("Final counter is:" ++ show cnt)


main :: IO ()
main = do
  announceTest "single threaded test"
  res <- runTVarIntReader adder2_10 0
  putStrLn ("Result (1+10*2?)" ++ show res)
  announceTest "Seconds counter multithreaded test"
  twoThreads
  announceTest "exit"

  return ()
 where
  announceTest str =
    putStrLn ("Press [Enter] for " ++ str) >> getLine >> return ()
