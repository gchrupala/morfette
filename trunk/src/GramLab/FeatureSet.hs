{-# LANGUAGE NoMonomorphismRestriction 
           , MultiParamTypeClasses 
           , FunctionalDependencies 
           , FlexibleContexts 
           , FlexibleInstances 
 #-} 
module GramLab.FeatureSet ( Feature (..)
                          , FeatureSet
                          , toFeatureSet
                          )
where
import GramLab.Intern
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.List   as List
import qualified GramLab.Utils as Utils
import Data.Maybe
import Data.Ord (comparing)

data Feature sym num = Sym sym
                     | Set [sym]
                     | Map [(sym,num)]
                     | Num num
                     | Null
                       deriving (Show,Read,Eq,Ord)

class FeatureSet coll key sym num | coll -> key sym num where
    toFeatureSet :: (Ord key,Ord sym,Real num) => 
                    coll -> State (Table (key,Maybe sym)) (IntMap.IntMap num)

instance FeatureSet [Feature sym num]               Int sym num where
    toFeatureSet = listToFeatureSet
instance FeatureSet [(key,Feature sym num)]         key sym num where
    toFeatureSet = assocListToFeatureSet
instance FeatureSet (Map.Map key (Feature sym num)) key sym num where
    toFeatureSet = mapToFeatureSet


assocListToFeatureSet = liftM (IntMap.fromList . concat) . mapM (uncurry realFeature) 
mapToFeatureSet = assocListToFeatureSet . Map.toList 
listToFeatureSet = assocListToFeatureSet . Utils.index

realFeature k Null     = return []
realFeature k (Sym s)  = intern (k,Just s)  >>= \i -> return $ [(i,1)]
realFeature k (Num n)  = intern (k,Nothing) >>= \i -> return $ [(i,n)]
realFeature k (Set ss) = mapM intern (zip (repeat k) (map Just (uniq ss))) >>= \is -> return $ zip is (repeat 1)
realFeature k (Map ss) = flip mapM (zip (repeat k) ss) $ \(k,(s,v)) -> do
                                      i <- intern (k,Just s)
                                      return (i,v)
uniq = Set.toList . Set.fromList 