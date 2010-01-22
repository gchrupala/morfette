{-# LANGUAGE NoMonomorphismRestriction #-}
module GramLab.Data.Diff.EditListRev ( make
                                     , apply
                                     , check
                                     , EditListRev(ELR)
                                     , SES.Edit(..)
                                  )
where
import qualified GramLab.Data.LCS.SimpleMemo as SES
newtype EditListRev a = ELR { un :: SES.EditScript a } deriving (Eq,Ord,Show,Read)
make w w' = ELR $ SES.ses (reverse w) (reverse w')
apply s w = reverse (SES.apply (un s) (reverse w))
check s w = SES.check (un s) (reverse w)

