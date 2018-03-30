# SharedState

Simple yet function implementation of MonadState that can be shared across multiple threads (unlike simple State Monad)
The implementation uses MonadReader with mutable environment (instance of MutableWrapper class)  
