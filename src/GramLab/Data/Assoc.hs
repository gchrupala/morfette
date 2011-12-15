{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies 
, FlexibleInstances #-}
module GramLab.Data.Assoc ( Assoc 
                          , fromAssoc
                          )
where

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import qualified Data.List   as List



class (Ord k) => Assoc assoc k v | assoc -> k v where
    toList   :: assoc -> [(k,v)]
    fromList :: [(k,v)]     -> assoc
    fromAssoc :: (Assoc assoc2 k v) => assoc -> assoc2
    fromAssoc = fromList . toList

instance (Ord k) => Assoc [(k,v)]           k   v where
    toList   = id 
    fromList = id

instance           Assoc (IntMap.IntMap v) Int v where
    toList   = IntMap.toAscList
    fromList = IntMap.fromList

instance (Ord k) => Assoc (Map.Map k v)     k   v where
    toList   = Map.toAscList
    fromList = Map.fromList
