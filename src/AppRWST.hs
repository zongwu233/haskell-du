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
type MyApp logEntry state = RWST AppEnv [logEntry] state IO

runMyApp :: MyApp logEntry state a -> AppConfig -> 
    state -> IO (a, [logEntry])
runMyApp app config st = evalRWST app (initialEnv config) st
