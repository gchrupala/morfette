
import GramLab.Data.Diff.EditTree2 hiding (split3)
import GramLab.Data.Diff.EditTree (split3)
import GramLab.Data.CommonSubstrings hiding (lcs)
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified GramLab.Data.StringLike as S

data FuzzyMatch s = FuzzyMatch s s [Int] [Int] Double deriving (Show,Eq,Ord)

data FuzzySplit s a = FuzzySplit { split  :: (Int,Int)
                                 , prefix :: EditTree s a
                                 , root   :: EditTree s a
                                 , suffix :: EditTree s a }
                    deriving (Show,Eq,Ord)


fuzzyScore (FuzzyMatch _ _ _ _ d) = d


substringDistances d xs ys = 
    sortBy (flip (comparing fuzzyScore)) 
               [ FuzzyMatch x y i_x i_y (d x y) | (x,i_x) <- (Map.toList . toMap . substrings) xs
                                                , (y,i_y) <- (Map.toList . toMap . substrings) ys ]

distance xs ys | head xs == head ys && last xs == last ys 
                   = fromIntegral ((length xs + length ys) - size (make xs ys)) / 2
               | otherwise = 0
lcs :: (Monad m, Ord a) => [a] -> [a] -> m (FuzzyMatch [a])
lcs xs ys = case substringDistances distance xs ys of
              (FuzzyMatch _ _ _ _ 0):_ -> fail "lcs: no fuzzy strings" 
              x:_                      -> return x

fuzzySplit xs ys = case lcs xs ys of 
                     Nothing  -> 
                         FuzzySplit { split  = (0,length xs)
                                    , prefix = make "" ""
                                    , root   = make xs ys
                                    , suffix = make "" "" }
                     Just (FuzzyMatch x y (i_x:_) (i_y:_) d) ->
                         let (xs_prefix,xs_root,xs_suffix) = split3 xs i_x (length xs - i_x - length x)
                             (ys_prefix,ys_root,ys_suffix) = split3 ys i_y (length ys - i_y - length y)
                         in FuzzySplit { split = (i_x,length xs - i_x - length x)
                                       , prefix = make xs_prefix ys_prefix
                                       , root   = make xs_root ys_root
                                       , suffix = make xs_suffix ys_suffix }

applyFuzzySplit s xs =    apply (prefix s) xs_prefix 
                       ++ apply (root   s) xs_root
                       ++ apply (suffix s) xs_suffix
    where (xs_prefix,xs_root,xs_suffix) = uncurry (split3 xs) (split s)
