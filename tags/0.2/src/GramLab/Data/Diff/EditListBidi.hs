{-# LANGUAGE NoMonomorphismRestriction #-}
module GramLab.Data.Diff.EditListBidi ( make
                                      , apply
                                      , check
                                      , EditListBidi(..)
                                      )
where
import qualified GramLab.Data.Diff.EditList as EL
import GramLab.Data.Diff.EditList (EditList)
import GramLab.Data.CommonSubstrings (lcs)
import Data.List

data EditListBidi a = ELB { prefixEditList  :: EditList a
                          , suffixEditList  :: EditList a } deriving (Eq,Ord,Show)



apply :: (Ord a) => EditListBidi a -> [a] -> [a]
apply ss w = rapply (suffixEditList ss) (EL.apply (prefixEditList ss) w)

check ss w = rcheck (suffixEditList ss) w && EL.check (prefixEditList ss) w

rcheck script = EL.check script . reverse 

rapply script = reverse . EL.apply script . reverse

make :: (Ord a) => [a] -> [a] -> EditListBidi a
make form lemma = ELB { prefixEditList  = EL.make preff prefl
                      , suffixEditList  = EL.make (reverse suff) (reverse sufl) }
    where (_,ixs) = maybe ([],([0],[0])) id (lcs form lemma)
          ix1     = head . sort . fst $ ixs
          ix2     = head . sort . snd $ ixs
          (preff,suff)   = splitAt ix1 form
          (prefl,sufl)   = splitAt ix2 lemma



