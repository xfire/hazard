module Hazard.Tests.TestUtils
    ( assertDirectoryContents
    , withTempDirectory
    , withSystemTempDirectory
    ) where

import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.List (sort)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive, createDirectory, getDirectoryContents, getTemporaryDirectory)
import System.FilePath((</>), joinPath, splitPath)
import Test.HUnit hiding (Test, path)

import System.Posix.Types (CPid)
import System.Posix.Process (getProcessID)
import Control.Exception (bracket)
import System.IO.Error (try, isAlreadyExistsError)


-- |Assert whether a directory contains exactly the given filenames.
assertDirectoryContents :: String      -- ^ Assert prefix
                        -> FilePath    -- ^ Directory to check
                        -> [FilePath]  -- ^ Expected directory content
                        -> Assertion
assertDirectoryContents prefix dir expected = do
        content <- getDirectoryContentsRecursive dir
        assertEqual prefix (cleanup expected) (cleanup content)
    where cleanup = sort . filter (`notElem` [".", ".."])


-- |Get the contents of a directory recursive by flattening the directory structure.
getDirectoryContentsRecursive :: FilePath        -- ^ Directory
                              -> IO [FilePath]
getDirectoryContentsRecursive p = liftM (addSpecialDirs . removeHead) (go p)
    where addSpecialDirs = (sd ++)
          removeHead = map (joinPath . (drop basePathSize) . splitPath)
          go path = do contents <- filter (`notElem` sd) <$> getDirectoryContents path
                       (files, dirs) <- partitionM ((liftM not) . doesDirectoryExist) $ map (path </>) contents
                       recDirs <- mapM go dirs
                       return $ files ++ dirs ++ (concat recDirs)
          sd = [".", ".."]
          basePathSize = length . splitPath $ p


-- |Monadic partition function.
--
-- Behaves exactly the same as 'Predef.partition', except it works in a monadic context.
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
    (t, f) <- partitionM p xs
    r <- p x
    if r then return (x:t,   f)
         else return (  t, x:f)

-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempDirectory :: String             -- ^ Directory name prefix
                        -> (FilePath -> IO a) -- ^ Callback that can use the directory
                        -> IO a
withSystemTempDirectory prefix action = do
    tmpDir <- getTemporaryDirectory 
    withTempDirectory tmpDir prefix action


-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the prefix. The temp directory is deleted after use. For example:
--
-- > withTempDirectory "src" "foo" $ \tmpDir -> do ...
withTempDirectory :: FilePath           -- ^ Directory to create the directory in
                  -> String             -- ^ Directory name prefix
                  -> (FilePath -> IO a) -- ^ Callback that can use the directory
                  -> IO a
withTempDirectory targetDir template =
  bracket
    (createTempDirectory targetDir template)
    (removeDirectoryRecursive)

-- | Try to create a new temporary directory.
--
-- Try to create a new temporary directory inside the given directory. The 
-- current process id and a random number is added to the new directory name.
-- 
-- If the new directory already exists, the creation is retried with a new
-- random number.
createTempDirectory :: FilePath    -- ^ Directory to create the directory in
                    -> String      -- ^ Directory name prefix
                    -> IO FilePath -- ^ Name of the newly created temporary directory
createTempDirectory base template = getProcessID >>= findTempName 0
  where findTempName :: Int -> CPid -> IO FilePath
        findTempName x pid = do
          let dirpath = base </> template ++ show pid ++ show x
          r <- try $ createDirectory dirpath
          case r of
            Right _ -> return dirpath
            Left  e | isAlreadyExistsError e -> findTempName (x + 1) pid
                    | otherwise              -> ioError e


