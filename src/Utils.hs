{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Utils(currentPathStatus
    ,traverseDirectoryWith
    ,checkExtension
    ) 
    where

import App
import System.Directory ( listDirectory )
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
    -- | liftIO can run an IO action in any monad stack based on the IO monad
    -- | FileStatus is definded using newtype, with a param 'path'
    -- | to get file info
    liftIO $ fileStatus path

-- | traverse all sub directory of MyApp currrent Path
-- | use local function execute app computation
traverseDirectoryWith :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith app = do
    currentPath <- asks path
    content <- liftIO $ listDirectory  currentPath
    -- | traverse_ : Map each element of a structure to an Applicative action, 
    -- | evaluate these actions from left to right, and ignore the results
    -- | traverse_ is just like mapM_, but generalised to Applicative actions.
    -- | traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
    -- | current traverse_ :: (FilePath -> MyApp le s () ) -> [FilePath] -> MyApp le s () 
    -- | current traverse_ ::  a -> b -> [a] -> b
    traverse_ go content -- Traverses over every file in the current directory
    where
        -- | NOTE: local function Execute a computation in a modified environment
        -- | execute a computation on app (here is dirTree) 
        -- | local :: (r -> r) -> m a -> m a
        go name  =  local (append name) app
        append name env = env {
                        path = path env </> name ,
                        depth = depth env +1
                    }

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg filePath = 
    -- `isExtensionOf` Does the given filename have the specified extension?
    maybe True (`isExtensionOf` filePath) (extension cfg)
