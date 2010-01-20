module GramLab.Data.Atom ( toAtom
                         , fromAtom )
where
import Prelude hiding(last,lookup)
import Data.IORef
import System.IO.Unsafe
import Data.HashTable
data Table a = T !Int (HashTable String Int) (HashTable Int String)
dict (T _ d _) = d
dictR (T _ _ d) = d
last (T i _ _) = i

{-# NOINLINE table #-}
table = unsafePerformIO (newIORef (T 0 (unsafePerformIO (new (==) hashString)) 
                                       (unsafePerformIO (new (==) hashInt))))

newtype Atom = A { un :: Int } deriving (Eq,Ord,Show,Read)

{-# NOINLINE toAtom #-}
toAtom :: String -> Atom
toAtom str = unsafePerformIO $ do 
  t <- readIORef table
  result <- lookup (dict t) str
  case result of
    Nothing -> do 
                insert (dict t) str (last t + 1) 
                insert (dictR t) (last t + 1) str
                writeIORef table (T (last t + 1) (dict t) (dictR t))
                return (A (last t + 1))
    Just i  -> do
                return (A i)
{-# NOINLINE fromAtom #-}
fromAtom :: Atom -> String
fromAtom a = unsafePerformIO $ do
  t <- readIORef table
  result <- lookup (dictR t) (un a)
  case result of
    Nothing -> error $ "GramLab.Data.Atom.fromAtom: wrong key: " ++ show a
    Just s  -> return s
           