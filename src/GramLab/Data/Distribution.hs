{-# OPTIONS_GHC -fglasgow-exts #-}

module GramLab.Data.Distribution
where
import GramLab.Data.Assoc
import Data.Map
import Data.IntMap

class (Assoc coll k Double) => Dist coll k where
    dist :: (Assoc coll1 k Double) => coll1 -> coll
    dist = fromAssoc
instance (Ord k) => Dist [(k,Double)] k
instance (Ord k) => Dist (Map k Double) k 
instance Dist (IntMap Double) Int
