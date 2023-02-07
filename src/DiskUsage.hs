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

-- | FileOffset actually is Int64
data DUEntryAction =
    TraverseDir { dirpath :: FilePath, requireReporting :: Bool }
    | RecordFileSize { fsize :: FileOffset }
    | None

-- | liftM2 :: forall (m :: * -> *) a1 a2 r.Monad m =>(a1 -> a2 -> r) -> m a1 -> m a2 -> m r
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
            -- | get state,which is FileOffset
            usageOnEntry <- get
            traverseDirectoryWith diskUsage
            when requireReporting $ do
                usageOnExit <- get
                -- | To compute the total space used by some directory, 
                -- | we have to find the difference between the total space used 
                -- | after leaving the directory and before entering it.
                tell [(dirpath, usageOnExit - usageOnEntry)]
        -- | modify state
        processEntry RecordFileSize{fsize}  = modify (+fsize)
        processEntry None = pure ()


