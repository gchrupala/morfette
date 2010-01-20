{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE TypeFamilies #-}
module GramLab.Data.AssocTF ( Assoc 
                            , fromAssoc
                            )
where

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import qualified Data.List   as List



class (Ord k) => Assoc a where
    type Key   a
    type Value a
    toList   :: a -> [(Key a,Value a)]
    fromList :: [(Key a,Value a)]     -> a
    fromAssoc :: (Assoc b) => a -> b
    fromAssoc = fromList . toList

instance (Ord k) => Assoc [(k,v)]          where
    type Key   [(k,v)] = k
    type Value [(k,v)] = v
    toList   = id 
    fromList = id

instance           Assoc (IntMap.IntMap v) where
    type (IntMap.IntMap v) = Int
    type (IntMap.IntMap v) = v
    toList   = IntMap.toAscList
    fromList = IntMap.fromList

instance (Ord k) => Assoc (Map.Map k v)    where
    type (Map.Map k v) = k
    type (Map.Map k v) = v
    toList   = Map.toAscList
    fromList = Map.fromList
