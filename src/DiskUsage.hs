{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module DiskUsage where

import App
import System.Posix.Types (FileOffset)
import Utils (currentPathStatus, checkExtension, traverseDirectoryWith)
import Control.Monad.RWS ( MonadReader(ask), MonadState(..) )
import System.PosixCompat ( isDirectory, fileSize )
import System.PosixCompat.Files (isRegularFile)
import Control.Monad ( when, liftM2 )
import Control.Monad.RWS.Lazy (MonadWriter(tell))
import Control.Monad.State (modify)

data DUEntryAction =
    TraverseDir { dirpath :: FilePath, requireReporting :: Bool }
    | RecordFileSize { fsize :: FileOffset }
    | None


diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
    where
        decide AppEnv{..} fs
            | isDirectory fs =
                TraverseDir path (depth <= maxDepth cfg)
            | isRegularFile fs && checkExtension cfg path =
                RecordFileSize (fileSize fs)
            | otherwise  = None

        processEntry TraverseDir{..} = do
            -- get state,which is FileOffset
            usageOnEntry <- get
            traverseDirectoryWith diskUsage
            when requireReporting $ do
                usageOnExit <- get
                -- To compute the total space used by some directory, 
                -- we have to find the difference between the total space used 
                -- after leaving the directory and before entering it.
                tell [(dirpath, usageOnExit - usageOnEntry)]
        -- modify state
        processEntry RecordFileSize{fsize}  = modify (+fsize)
        processEntry None = pure ()


