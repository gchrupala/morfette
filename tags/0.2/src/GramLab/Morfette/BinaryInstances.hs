module GramLab.Morfette.BinaryInstances 
where
import GramLab.Data.Diff.EditListRev
import GramLab.Data.Diff.EditTree 
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
    put (Split i j lt rt) = put (1::Word8) >> put i >> put j 
                            >> put lt >> put rt
    get = do
      tag <- get
      case tag::Word8 of
        0 -> liftM2 Replace get get
        1 -> liftM4 Split   get get get get

instance Binary a => Binary (EditListRev a) where
    put (ELR x) = put x
    get = liftM ELR get
