{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module DirTree(
    dirTree,
    treeEntryBuilder
) where
import App
import Utils(currentPathStatus, traverseDirectoryWith)
import Control.Monad.RWS.Class (MonadReader(ask))
import System.PosixCompat.Files (isDirectory)
import Control.Monad.RWS (MonadWriter(tell),when)
import System.FilePath.Posix (takeBaseName)
import Data.Text.Lazy.Builder ( fromString, Builder )
dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
    -- can use AppEnv all field var in follow code  
    AppEnv{..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        tell [(takeBaseName path, depth)]
        traverseDirectoryWith dirTree

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp,n) = fromString indent <> fromString fp
    where 
        indent = replicate (2*n) ' '