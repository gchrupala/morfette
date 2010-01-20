{-# LANGUAGE TypeFamilies #-}
module ProbDist 
where


import qualified Data.Map as Map
import qualified Data.MultiSet as MS

type Dist a = Map.Map a Double

class ToDist a where
    type Elt a 
    toDist :: a -> Dist (Elt a)

instance ToDist (MS.MultiSet a) where
    type Elt (MS.MultiSet a) = a
    toDist = toDist . MS.toMap


instance ToDist (Map.Map a Int) where
    type Elt (Map.Map a Int) = a
    toDist m = Map.map (\e -> fromIntegral e / fromIntegral c) m
        where c = Map.size m

instance Ord a => ToDist [a] where
    type Elt [a] = a
    toDist = toDist . MS.fromList

