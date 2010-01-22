module GramLab.Data.Diff.FuzzyEditTree ( make
                                       , apply
                                       , check
                                       )
where

import GramLab.Data.Diff.EditTree  (split3)
import qualified GramLab.Data.Diff.EditTree2 as ET2
import qualified GramLab.Data.Diff.EditTree2Rev as ET2Rev
import GramLab.Data.CommonSubstrings hiding (lcs)
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified GramLab.Data.StringLike as S
import Test.QuickCheck hiding (check)
import Data.Char

make  = fuzzySplit
apply = applyFuzzySplit
check  s w = let (w_prefix,w_root,w_suffix) = uncurry (split3 w) (split s)
             in     ET2.check (prefix s) w_prefix
                 && ET2Rev.check (root   s) w_root
                 && ET2Rev.check (suffix s) w_suffix


data FuzzyMatch s = FuzzyMatch s s [Int] [Int] Double deriving (Show,Eq,Ord)

data FuzzySplit s a = FuzzySplit { split  :: (Int,Int)
                                 , prefix :: ET2.EditTree s a
                                 , root   :: ET2Rev.EditTree s a
                                 , suffix :: ET2Rev.EditTree s a }
                    deriving (Show,Eq,Ord)


fuzzyScore (FuzzyMatch _ _ _ _ d) = d


substringDistances d xs ys = 
    sortBy (flip (comparing fuzzyScore)) 
               [ FuzzyMatch x y i_x i_y (d x y) | (x,i_x) <- (Map.toList . toMap . substrings) xs 
                                                , (y,i_y) <- (Map.toList . toMap . substrings) ys 
                                                , head x == head y && last x == last y             ]

  


distance xs ys = fromIntegral ((length xs + length ys) - ET2Rev.size (ET2Rev.make xs ys)) / 2
               
lcs :: (Monad m, Ord a) => [a] -> [a] -> m (FuzzyMatch [a])
lcs xs ys = case substringDistances distance xs ys of
              []                       -> fail "lcs: no fuzzy strings"
              (FuzzyMatch _ _ _ _ 0):_ -> fail "lcs: no fuzzy strings" 
              x:_                      -> return x

fuzzySplit xs ys = case lcs xs ys of 
                     Nothing  -> 
                         FuzzySplit { split  = (0,length xs)
                                    , prefix = ET2.make "" ""
                                    , root   = ET2Rev.make "" ""
                                    , suffix = ET2Rev.make xs ys }
                     Just (FuzzyMatch x y (i_x:_) (i_y:_) d) ->
                         let (xs_prefix,xs_root,xs_suffix) = split3 xs i_x (length xs - i_x - length x)
                             (ys_prefix,ys_root,ys_suffix) = split3 ys i_y (length ys - i_y - length y)
                         in FuzzySplit { split = (i_x,length xs - i_x - length x)
                                       , prefix = ET2.make xs_prefix ys_prefix
                                       , root   = ET2Rev.make xs_root ys_root
                                       , suffix = ET2Rev.make xs_suffix ys_suffix }

applyFuzzySplit s xs =    ET2.apply (prefix s) xs_prefix 
                       ++ ET2Rev.apply (root   s) xs_root
                       ++ ET2Rev.apply (suffix s) xs_suffix
    where (xs_prefix,xs_root,xs_suffix) = uncurry (split3 xs) (split s)
-- Quickchecks props are the same for all edit script types
-- runtest should be in a separate module taking a record of functions
-- to test against these properties

instance Arbitrary Char where                                                                            
    arbitrary     = choose ('a','d')                                                                     
    coarbitrary c = variant (ord c `rem` 4)   

prop_apply :: (String,String) -> Bool                                                                    
prop_apply  (w,w') = w' == apply (make w w') w 

prop_make_apply_rev :: (String,String) -> Bool
prop_make_apply_rev (w,w') = reverse (apply (make (reverse w) (reverse w')) (reverse w)) == w'

prop_check :: (String,String) -> Bool
prop_check (w,w') = check (make w w') w

runtests = do
  q  "apply" prop_apply
  q  "make_apply_rev" prop_make_apply_rev
  q  "check" prop_check
 where q str prop = do putStr $ str ++ " "
                       quickCheck prop
