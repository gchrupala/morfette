module GramLab.Morfette.BinaryInstances 
where
import GramLab.Data.Diff.EditListRev
import GramLab.Data.Diff.EditTree 
import GramLab.Data.Diff.EditListBidi 
import qualified GramLab.Data.Diff.EditTree2Rev as ET2R
import Data.Binary
import Control.Monad (liftM,liftM2,liftM3,liftM4)
instance Binary a => Binary (Edit a) where
    put (Ins c i) = put (0::Word8) >> put c >> put i
    put (Del c i) = put (1::Word8) >> put c >> put i
    get = do 
      tag <- get
      case tag::Word8 of
        0 -> liftM2 Ins get get
        1 -> liftM2 Del get get

instance Binary s => Binary (EditTree s a) where
    put (Replace xs ys)  = put (0::Word8) >> put xs >> put ys
    put (Split i j lt rt) = put (1::Word8) >> put i >> put j >> put lt >> put rt
    get = do
      tag <- get
      case tag::Word8 of
        0 -> liftM2 Replace get get
        1 -> liftM4 Split   get get get get

instance Binary a => Binary (EditListRev a) where
    put (ELR x) = put x
    get = liftM ELR get
{- FIXME the duplicate definition 
    Duplicate instance declarations:
      instance (Binary s) => Binary (ET2R.EditTree s a)
	-- Defined at /home/gchrupala/work/gramlab/morfette/src/GramLab/Morfette/BinaryInstances.hs:(30,0)-(32,27)
      instance (Binary s) => Binary (ET2R.EditTree s a)
	-- Defined in GramLab.Data.Diff.FuzzyEditTree
instance Binary s => Binary (ET2R.EditTree s a) where
    put (ET2R.ETR x) = put x
    get = liftM ET2R.ETR get
-}
instance Binary a => Binary (EditListBidi a) where
    put (ELB x y) = put x >> put y
    get = liftM2 ELB get get
