# SharedState

Simple, yet function implementation of MonadState that can be shared across multiple threads (unlike simple State Monad)
The implementation uses MonadReader with mutable environment (instance of MutableWrapper class):  
```
class (Monad m) => MutableWrapper w m where
  update :: (s -> (a , s)) -> w s -> m a
```

Here is instance for STM TVar and MonadIO:
```
instance (MonadIO m) => MutableWrapper TVar m where
  update f ts =
    liftIO $ atomically $ do
      s <- readTVar ts
      let (a, s1) = f s
      writeTVar ts s1
      return a
```

Main.hs demonstrates usage of SharedState. It uses the following newtype that auto derives all required classes (thanks to `GeneralizedNewtypeDeriving`):
```
newtype TVarMonadState s a = TVarMonadState (ReaderT (TVar s) IO a)
 deriving (Functor, Applicative, Monad, MonadReader (TVar s), MonadIO)
```

as result it also became instance of `MonadState s IO` so following functions are available:
- modify
- get
- put

Inherently, when using TVar based MutableWrapper update is thread safe, but if when using get and then put value can be overridden from different threads.

While currently not provided here additional implementation of MutableWrapper are possible including `IORef` and `MVar`  
