{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Utils(currentPathStatus
    ,traverseDirectoryWith
    ,checkExtension
    ) 
    where

import App
import System.Directory
import System.PosixCompat (FileStatus)
import Control.Monad.RWS (MonadReader(ask), MonadIO (liftIO))
import AppTypes (AppEnv(..))
import Control.Monad.Reader (asks)
import Control.Monad.RWS.Lazy (MonadReader(..))
import System.FilePath ( (</>) )
import Data.Foldable (traverse_)
import System.FilePath.Posix (isExtensionOf)

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
    AppEnv { fileStatus,path } <- ask
    -- liftIO can run an IO action in any monad stack based on the IO monad
    -- FileStatus is definded using newtype, with a param 'path'
    -- to get file info
    liftIO $ fileStatus path


traverseDirectoryWith :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith app = do
    asks path >>= liftIO . listDirectory  >>= traverse_ go 
    where
        -- local :: (r -> r) -> RWST r w s m a -> RWST r w s m a
        go name  =  local (append name)  app
        append name env = env {
                        path = path env </> name ,
                        depth = depth env +1
                    }

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg filePath = 
    -- `isExtensionOf` Does the given filename have the specified extension?
    maybe True (`isExtensionOf` filePath) (extension cfg)
