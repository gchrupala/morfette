module GramLab.Morfette.NFDataIntances 
where

import Control.Parallel.Strategies
import GramLab.Morfette.Features.Common
import GramLab.Data.Diff.EditListRev
instance (NFData a, NFData b) => NFData (OR a b) where
    rnf (LHS a) = rnf a
    rnf (RHS b) = rnf b
    rnf (Both a b) = rnf a `seq` rnf b `seq` ()
instance (NFData a) => NFData (EditListRev a) where
    rnf (ELR a) = rnf a
instance (NFData a) => NFData (Edit a) where
    rnf (Ins x y) = rnf x `seq` rnf y `seq` rnf ()
    rnf (Del x y) = rnf x `seq` rnf y `seq` rnf ()
