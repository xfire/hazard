module Hazard.Tests.TestUtils
    ( initOutputDirectory
    , assertDirectoryContents
    ) where

import Control.Monad (when, liftM)
import Control.Applicative ((<$>))
import Data.List (sort)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive, createDirectory, getDirectoryContents)
import System.FilePath((</>), joinPath, splitPath)
import Test.HUnit hiding (Test, path)

-- |Initialize the output directory. If necessary remove it and then create a new one.
initOutputDirectory :: FilePath   -- ^ Directory
                    -> IO ()
initOutputDirectory dir = do
    isDir <- doesDirectoryExist dir
    when (isDir) $ removeDirectoryRecursive dir
    createDirectory dir


-- |Assert whether a directory contains exactly the given filenames.
assertDirectoryContents :: String      -- ^ Assert prefix
                        -> FilePath    -- ^ Directory to check
                        -> [FilePath]  -- ^ Expected directory content
                        -> Assertion
assertDirectoryContents prefix dir expected = do
        content <- getDirectoryContentsRecursive dir
        assertEqual prefix (cleanup expected) (cleanup content)
    where cleanup = sort . filter (`notElem` [".", ".."])


-- |Get the contents of a directory recursive. e.g. flattening the directory structure.
getDirectoryContentsRecursive :: FilePath        -- ^ Directory
                              -> IO [FilePath]
getDirectoryContentsRecursive p = liftM (addSpecialDirs . removeHead) (go p)
    where addSpecialDirs = (sd ++)
          removeHead = map (joinPath . tail . splitPath)
          go path = do contents <- filter (`notElem` sd) <$> getDirectoryContents path
                       (files, dirs) <- partitionM ((liftM not) . doesDirectoryExist) $ map (path </>) contents
                       recDirs <- mapM go dirs
                       return $ files ++ dirs ++ (concat recDirs)
          sd = [".", ".."]


-- |Monadic partition function.
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
    (t, f) <- partitionM p xs
    r <- p x
    if r then return (x:t,   f)
         else return (  t, x:f)
