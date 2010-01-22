{-# OPTIONS -fglasgow-exts #-}
module GramLab.Composable (Composable,compose)
where
import qualified Data.Map as Map

class Composable f a b where
    compose ::  f b c -> f a b -> f a c
                                    

instance (Ord a,Ord b) => Composable Map.Map a b where
    compose m1 m2 = Map.foldWithKey (\k v m' -> case Map.lookup v m1 of 
                                                  { Just v' -> Map.insert k v' m'
                                                  ; Nothing -> m' } ) Map.empty m2
                                                                     
instance Composable (->) a b where
    compose f g = f . g
