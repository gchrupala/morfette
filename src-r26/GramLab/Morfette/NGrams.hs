--module GramLab.Morfette.NGrams ( )
--where

import qualified Data.ByteString.Lazy.Char8 as S

--import qualified Data.List as S
import Data.List
import Data.Char
import qualified GramLab.Data.MultiSet as MS
import qualified Data.Map as Map
--import GramLab.Utils
import GramLab.Morfette.LZipper
import Data.Binary
import System 

sepPunct xs = let (before,rest) = S.span isPunctuation xs
                  (this,after)  = S.span (not . isPunctuation) rest
              in  filter (not . S.null) [before,this,after]

tokenize = concatMap sepPunct . S.words

toNGrams i z | atEnd z = []
toNGrams i z = let Just w = focus z 
               in (w, flatten (take i (left z)) (take i (right z))):toNGrams i (slide z)


flatten l r = (l,r) --S.concat [S.unwords l,S.pack "_",S.unwords r]
main = do
  [n] <- getArgs
  txt <- S.getContents
  let paragraphs = map S.unlines (splitWith S.null (S.lines txt))
      ngs = concatMap (\p -> toNGrams (read n) (fromList . tokenize $ txt)) paragraphs
  flip mapM_ ngs $ \(w,(b,a)) -> S.putStrLn (S.unwords [w, S.unwords b, S.pack "__", S.unwords a])
  

splitWith ::  (a -> Bool) -> [a] -> [[a]]
splitWith f s =  case dropWhile f s of
                   [] -> []
                   s' -> w : splitWith f s''
                       where (w, s'') = break f s'

instance (Binary a, Ord a) => Binary (MS.MultiSet a) where
    put ms = put (MS.toAscOccurList ms)
    get = do
      xs <- get
      return $ MS.fromAscOccurList xs