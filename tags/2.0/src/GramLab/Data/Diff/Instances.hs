{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module GramLab.Data.Diff.Instances ( Diff (..) 
                                   , editList
                                   , editListRev
                                   , editListBidi
                                   , editTree
                                   )
                                   
where
import qualified GramLab.Data.Diff.EditList as EL
import qualified GramLab.Data.Diff.EditListRev as ELR
import qualified GramLab.Data.Diff.EditListBidi as ELB
import qualified GramLab.Data.Diff.EditTree as ET

-- FIXME adapt to StringLike
class Diff d a | d -> a where
    make  :: d -> [a] -> [a] -> d
    apply :: d -> [a] -> [a]
    check :: d -> [a] -> Bool
 
instance (Ord a,Ord s) => Diff (ET.EditTree s a) a   where
    make _ = ET.make
    apply  = ET.apply
    check  = ET.check
instance (Ord a) => Diff (EL.EditList a) a where
    make _ = EL.make
    apply  = EL.apply
    check  = EL.check
instance (Ord a) => Diff (ELR.EditListRev a) a where
    make _ = ELR.make
    apply  = ELR.apply
    check  = ELR.check
instance (Ord a) => Diff (ELB.EditListBidi a) a where
    make _ = ELB.make
    apply  = ELB.apply
    check  = ELB.check

-- witnesses
editList     :: (Ord a) => EL.EditList a
editList     = undefined
editListRev  :: (Ord a) => ELR.EditListRev a
editListRev  = undefined
editListBidi :: (Ord a) => ELB.EditListBidi a
editListBidi = undefined
editTree     :: (Ord a,Ord s) => ET.EditTree s a
editTree     = undefined
