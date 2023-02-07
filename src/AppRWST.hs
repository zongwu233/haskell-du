{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AppRWST (
    MyApp(..),
    runMyApp
) where
import Control.Monad.RWS ( RWST )
import AppTypes(AppEnv, AppConfig,initialEnv)
import Control.Monad.RWS.Lazy (evalRWST)

-- newtype RWST r w s m a ,so you can use reader,writer,state functionality
-- newtype RWST r w s m a = RWST { unRWST :: r -> s -> w -> m (a, s, w) }
-- RWST has ReaderT WriterT StateT
type MyApp logEntry state = RWST AppEnv [logEntry] state IO

runMyApp :: MyApp logEntry state a -> AppConfig -> 
    state -> IO (a, [logEntry])

-- evalRWST :: forall (m :: * -> *) r w s a.Monad m => RWST r w s m a -> r -> s -> m (a, w)
-- Evaluate a computation with the given initial state and environment, 
-- returning the final value and output, discarding the final state.
runMyApp app config st = evalRWST app (initialEnv config) st
