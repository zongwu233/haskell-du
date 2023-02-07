{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DirTree(
    dirTree,
) where
import App
import Utils(currentPathStatus, traverseDirectoryWith)
import Control.Monad.RWS.Class (MonadReader(ask))
import System.PosixCompat.Files (isDirectory)
import Control.Monad.RWS (MonadWriter(tell), MonadIO (liftIO))
import System.FilePath.Posix (takeBaseName)
import Data.Text.Lazy.Builder ( fromString, Builder )
import Control.Monad (when)

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
    -- | can use AppEnv all field var in follow code  
    AppEnv{..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        tell [(takeBaseName path, depth)]
        traverseDirectoryWith dirTree
