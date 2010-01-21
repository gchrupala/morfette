{-# OPTIONS -fno-monomorphism-restriction #-}
module GramLab.BinarizeFeatures ( convertExamples
                                , ClassOrFeat(..)
                                , inverseClassMap
                                , maybeConvertExamples
                                , maybeConvertExample
                                , showExample
                                , writeTable
                                , readTable
                                )
where
import Control.Monad.State
import Data.Maybe
import GramLab.Intern
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.Binary hiding (put,get)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as B
import GramLab.Utils (index)
import qualified Data.Foldable as F
import Control.Arrow (second)

data ClassOrFeat a b = Class a | Feat (Int, b) deriving (Show,Read,Eq,Ord)

prepareFeats = fmap (Feat . second fromJust) . filter (isJust . snd) . index

binarizeVector = liftM (IntSet.fromList . F.toList) . internMany . prepareFeats


binarizeVectors = mapM binarizeVector

maybeBinarizeVector = liftM (IntSet.fromList . catMaybes . F.toList ) . maybeInternMany . prepareFeats


maybeBinarizeVectors = mapM maybeBinarizeVector


internClass = intern . Class


internClasses =  mapM internClass


convertExample (k,fs) = liftM2 (,) (internClass k) (binarizeVector fs)


convertExamples = mapM convertExample


maybeConvertExample (k,fs) = liftM2 (,) (internClass k) (maybeBinarizeVector fs)


maybeConvertExamples = mapM maybeConvertExample

showExample :: (Show t) => (t, IntSet.IntSet) -> String
showExample (k,xs) = unwords $ show k : map ((++ ":1") . show) (IntSet.toAscList xs)

invertMap = Map.foldWithKey (\k v m' -> Map.insert v k m') Map.empty

inverseClassMap :: (Ord a, Ord b) => Map.Map (ClassOrFeat a b) Int -> Map.Map Int a
inverseClassMap  =   Map.map (\(Class x) -> x) 
                   . invertMap 
                   . Map.mapMaybeWithKey (\k x -> case k of { Class y -> Just x ; _ -> Nothing }) 

instance (Ord a, Binary a) => Binary (Table a) where
    put (T i m) = Bin.put i >> Bin.put m
    get = liftM2 T Bin.get Bin.get

instance (Binary a, Binary b) => Binary (ClassOrFeat a b) where
    put (Class a) = Bin.put (0 :: Word8) >> Bin.put a
    put (Feat  b) = Bin.put (1 :: Word8) >> Bin.put b
    get = do tag <- getWord8
             case tag of
               0 -> liftM Class Bin.get
               1 -> liftM Feat  Bin.get


writeTable t@(T _ _) path = do
  B.writeFile path (encode t)


readTable path = do
  str <- B.readFile path
  return (decode str)
