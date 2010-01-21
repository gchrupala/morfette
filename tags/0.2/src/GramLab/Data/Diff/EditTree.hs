{-# LANGUAGE NoMonomorphismRestriction #-}
module GramLab.Data.Diff.EditTree ( make
                                  , apply
                                  , check
                                  , split3
                                  , EditTree(..)
                                  )
where

import GramLab.Data.CommonSubstrings 
import qualified GramLab.Data.StringLike as S
import Data.Maybe (fromMaybe)
import Debug.Trace

make = editTree

data EditTree s a = Split !Int !Int (EditTree s a) (EditTree s a)
                  | Replace s s
                    deriving (Eq,Ord,Show,Read)
editTree w w' = case lcsi w w' of
                  Nothing -> Replace w w'
                  Just (i_w,i_w_end,i_w',i_w'_end) -> 
                      let (w_prefix, w_root, w_suffix)  = split3 w   i_w  i_w_end
                          (w'_prefix,w'_root, w'_suffix) = split3 w' i_w' i_w'_end
                      in  Split i_w i_w_end
                                (editTree w_prefix  w'_prefix)
                                (editTree w_suffix  w'_suffix)

lcsi w w' = fmap f (lcs w w') 
    where f (str,(i_w:_,i_w':_)) = (i_w,i_w_end,i_w',i_w'_end)
              where i_w_end  = S.length w  - i_w  - len
                    i_w'_end = S.length w' - i_w' - len
                    len      = S.length str

apply (Replace s s') w = s'
apply (Split i i_end lt rt) w = (apply lt pre) `S.append` root `S.append` (apply rt suf)
    where (pre,root,suf) = split3 w i i_end
                                              

split3 w i i_end = let (prefix, rest)  = S.splitAt i w 
                       (suffix_r, root_r)  = S.splitAt i_end (S.reverse rest)
                   in (prefix,(S.reverse root_r),(S.reverse suffix_r))


check (Replace s s') w       = s == w
check (Split i j lt rt) w  =      len >= i 
                               && len >= j 
                               && check lt w_pre 
                               && check rt w_suf
    where len = S.length w 
          (w_pre,w_root,w_suf) = split3 w i j

