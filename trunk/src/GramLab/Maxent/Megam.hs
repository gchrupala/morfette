module Megam () where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (transpose,foldl')
import Prelude hiding (sum)
import GramLab.FeatureSet
import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
import GramLab.Intern
import System.Process
import System.IO
phi "cat"       = [1, 0          , 0         , 1          , 1 ]
phi "platypus"  = [1, 0          , 1         , 1          , 1 ]
phi "chicken"   = [1, 1          , 1         , 0          , 0 ]


xs `dot` ys = sum (zipWith (*) xs ys)
sum = foldl' (+) 0

p ys ws y x = exp (ws y `dot` phi x) / sum [ exp (ws y' `dot` phi x) | y' <- ys ]

readWeights string  = Map.fromList $ zip [0..] (transpose . map parseLine . lines $ string)

parseLine :: String -> [Double]
parseLine  = map read . tail . words 

data MegamTrainSettings = MegamTS deriving (Show)


data ModelData lab key sym num = ModelData { classMap   :: Table lab
                                           , featureMap :: Table (key,Maybe sym) 
                                           , scalingMap :: IntMap.IntMap num 
                                           , settings :: MegamTrainSettings
                                           , inverseClassMap   :: IntMap.IntMap lab
                                           } deriving (Show)
data Model lab key sym num = Model { model      :: Map.Map Int [Double]
                                   , modelData  :: ModelData lab key sym num }

scaleFeatureSet :: (Fractional a, Ord a) => IntMap.IntMap a -> IntMap.IntMap a -> IntMap.IntMap a
scaleFeatureSet  maxVals = IntMap.mapWithKey (\k v -> v/IntMap.findWithDefault 1 k maxVals)
invertMap = Map.foldWithKey (\k v m' -> IntMap.insert v k m') IntMap.empty        
maxValues = IntMap.unionsWith max


train params examples = do
  m <- megamTrain params  samples 
  return $ Model { model       = m 
                 , modelData   = ModelData { classMap    = cm 
                                           , featureMap  = fm 
                                           , scalingMap  = sm 
                                           , settings    = params
                                           , inverseClassMap   = invertMap $ let (T _ icm) = cm   in icm
                                           }}
  where (ks,xs)       = unzip examples
        (labels,cm)   = runState (mapM intern ks)       initial
        (featsets,fm) = runState (mapM toFeatureSet xs) initial
        sm            = maxValues featsets
        samples       = zipWith (\l fs -> (l,IntMap.toList fs)) 
                                labels 
                                ((if False {-scale params-} then map (scaleFeatureSet sm) else id) featsets)

megamTrain params samples = do  
  dump samples "train"
  (inp,oup,err,pid) <- runInteractiveCommand "megam_i686.opt -fvals multiclass train"
  txt<- hGetContents oup 
  length txt `seq` waitForProcess pid; 
  return (readWeights txt)

dump = undefined