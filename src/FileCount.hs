{-# LANGUAGE RecordWildCards #-}

module FileCount where 

import App
import Control.Monad (when)
import System.PosixCompat (isDirectory)
import Control.Monad.RWS (MonadReader(ask))
import Utils (currentPathStatus, traverseDirectoryWith, checkExtension)
import System.Directory.Extra (listFiles)
import Control.Monad.RWS.Class (MonadWriter(tell))
import Control.Monad.IO.Class (MonadIO(..))

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
     AppEnv {..} <- ask
     fs <- currentPathStatus
     when (isDirectory fs && depth <= maxDepth cfg )$ do
        files <- liftIO $ listFiles path
        tell [(path, length $ filter (checkExtension cfg) files)]
        traverseDirectoryWith fileCount
    

