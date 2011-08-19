module Hazard.Core
    ( HConfig(..)
    , hazard
    ) where

import Data.List (isPrefixOf)
import Control.Monad (when, forM_)
import Control.Applicative ((<$>))
import System.FilePath((</>))
import System.Directory (doesDirectoryExist, createDirectory, getDirectoryContents, copyFile)

data HConfig = HConfig
    { inputDir  :: String
    , outputDir :: String
    }


-- |Hazard's main entry point.
hazard :: HConfig -> IO ()
hazard cfg = do
    copyFiles (inputDir cfg) (outputDir cfg)
    return ()


-- |Copy files and directories from the source to the destination directory.
copyFiles :: FilePath   -- ^ Source directory
          -> FilePath   -- ^ Destination directory
          -> IO ()
copyFiles src dst = do
        dstExists <- doesDirectoryExist dst 
        when (not dstExists) $ createDirectory dst

        contents <- cntFilter <$> getDirectoryContents src

        forM_ contents $ \name -> do
            let srcPath = src </> name
            let dstPath = dst </> name
            isDirectory <- doesDirectoryExist srcPath
            if isDirectory then copyFiles srcPath dstPath
                           else copyFile srcPath dstPath

    where cntFilter = (filter (`notElem` [".", ".."])) . (filter (noPrefixOf "_")) 
          noPrefixOf x y = not $ isPrefixOf x y
