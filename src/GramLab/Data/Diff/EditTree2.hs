{-# LANGUAGE NoMonomorphismRestriction #-}
module GramLab.Data.Diff.EditTree2 ( make
                                   , apply
                                   , size
                                   , check
                                   , EditTree(..)
                                   )
where

import GramLab.Data.CommonSubstrings 
import qualified GramLab.Data.StringLike as S
import Data.Maybe (fromMaybe)
import Debug.Trace


make = editTree


data EditTree s a = Split !Int (Maybe Int) (EditTree s a) (EditTree s a)
                  | Replace s s
                    deriving (Eq,Ord,Show)
                              
editTree w w' = case lcsi w w' of
                   Nothing  -> Replace w w'
                   Just (i_w,j_w,i_w',j_w') -> 
                       let j'_w  = fromMaybe (length w)  j_w
                           j'_w' = fromMaybe (length w') j_w'
                           (w_prefix, w_root, w_suffix) = split3 w  i_w  j'_w
                           (w'_prefix,w'_root, w'_suffix) = split3 w' i_w' j'_w'
                       in Split i_w j_w
                                (editTree w_prefix  w'_prefix)
                                (editTree w_suffix  w'_suffix)
lcsi w w' = fmap f (lcs w w')
    where f (str,(i_w:_,i_w':_)) = (i_w,i_w_end,i_w',i_w'_end)
              where i_w_end  = if i_w  + len == length w  then Nothing else Just $ i_w  + len
                    i_w'_end = if i_w' + len == length w' then Nothing else Just $ i_w' + len
                    len      = S.length str


apply = apply'


apply' (Replace s s') w = s'
apply' (Split i i_end lt rt) w = (apply' lt pre) `S.append` root `S.append` (apply' rt suf)
    where (pre,root,suf) = split3 w i j
          j = fromMaybe (S.length w) i_end
          
                                              


split3 w i j = let (prefix,rest) = S.splitAt i w
                   (root,suffix) = S.splitAt (j-i) rest
               in  (prefix,root,suffix)

check (Replace s s') w       = s == w
check (Split i mj lt rt) w  =     len >= i 
                               && len >= j 
                               && check lt w_pre 
                               && check rt w_suf
    where len = S.length w 
          (w_pre,w_root,w_suf) = split3 w i j
          j = fromMaybe len mj



size (Replace s s') = length s + length s'
size (Split _ _ lt rt) = size lt + size rt

