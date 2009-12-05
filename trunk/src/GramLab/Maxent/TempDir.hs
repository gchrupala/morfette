module GramLab.Maxent.TempDir (withTempFilePath)
where
import System.Directory
import System.FilePath
import Control.Exception (bracket)
import Data.Char
import System.Random
import Debug.Trace
withTempFilePath prefix f = bracket (do tempdir <- getTemporaryDirectory
                                        dir <- randomDirName prefix
                                        let path = tempdir </> dir 
                                        createDirectory path
                                        return $ path </> "data")
                                    (removeDirectoryRecursive . takeDirectory)
                                    f
-- helpers
mkRandomString :: Int -> (Char -> Bool) -> IO String
mkRandomString i f = do
  g <- newStdGen
  return $ take i . filter f $ randomRs ('0','z') g


randomDirName p = do 
  suffix <- mkRandomString 10 isAlphaNum
  return $ p ++ suffix
