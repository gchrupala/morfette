module GramLab.Maxent.ZhangLe.Model ( train
                                    , distribution
                                    , classify
                                    , save
                                    , load
                                    , I.trainSettings
                                    , I.TrainSettings(..)
                                    , Model(..)
                                    , ModelData(..)
                                    , dump
                                    , dumpMapping
                                    , DumpMode(..)
                                    )
where
import qualified GramLab.Maxent.ZhangLe.IntModel as I
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary.Strict as B
import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
import System.FilePath
import System.Directory
import Control.Exception (bracket)
import System.Random
import Data.Char (isAlphaNum)
import GramLab.Intern
import GramLab.Data.Assoc
import GramLab.FeatureSet
import System.IO

type Example label features = (label,features)
data ModelData lab key sym num = ModelData { classMap   :: Table lab
                                           , featureMap :: Table (key,Maybe sym) 
                                           , scalingMap :: IntMap.IntMap num 
                                           , settings   :: I.TrainSettings 
                                           , inverseClassMap   :: IntMap.IntMap lab
                                           --, inverseFeatureMap :: IntMap.IntMap (key,Maybe sym) 
                                           } deriving (Show)
data Model lab key sym num = Model { model      :: I.IntModel
                                   , modelData  :: ModelData lab key sym num }

scaleFeatureSet :: (Fractional a, Ord a) => IntMap.IntMap a -> IntMap.IntMap a -> IntMap.IntMap a
scaleFeatureSet  maxVals = IntMap.mapWithKey (\k v -> v/IntMap.findWithDefault 1 k maxVals)
invertMap = Map.foldWithKey (\k v m' -> IntMap.insert v k m') IntMap.empty        
maxValues = IntMap.unionsWith max

train p examples = do
  m <- I.train p  samples --(trace (unlines $ map show samples) samples)
  return $ Model { model       = m 
                 , modelData   = ModelData { classMap    = cm 
                                           , featureMap  = fm 
                                           , scalingMap  = sm 
                                           , settings    = p   
                                           , inverseClassMap   = invertMap $ let (T _ icm) = cm   in icm
                                           --, inverseFeatureMap = invertMap $ let (T _ ifm) = fm   in ifm
                                           }}
  where (ks,xs)       = unzip examples
        (labels,cm)   = runState (mapM intern ks)       initial
        (featsets,fm) = runState (mapM toFeatureSet xs) initial
        sm            = maxValues featsets
        samples       = zipWith (\l fs -> (l,IntMap.toList fs)) 
                                labels 
                                ((if I.scale p then map (scaleFeatureSet sm) else id) featsets)

data DumpMode = Write FilePath | Read FilePath | Skip deriving (Show,Read)
dump h examples = dumpMapping h Skip examples


dumpMapping h dm examples = do 
  (cm,fm) <- case dm of
               Read p -> do
                      s <- BS.readFile p
                      return (B.decode s)
               _      -> return (initial,initial)
  let (ks,xs)       = unzip examples
      (labels,cm')   = runState (mapM intern ks)       cm
      (featsets,fm') = runState (mapM toFeatureSet xs) fm
      sm            = maxValues featsets
      samples       = zipWith (\l fs -> (l,IntMap.toList fs)) 
                            labels 
                            featsets
      format (l,fs) = unwords (show l : map (\(f,v) -> show f ++ ":" ++ show v) fs)
  mapM_ (hPutStrLn h . format) samples
  case dm of
    Write p -> BS.writeFile p (B.encode (cm',fm'))
    _       -> return ()


distribution m fs = fromAssoc $ map (\(k,v) -> (origClass k,v)) dist
    where dist    = I.evalAll (model m) s
          s       = IntMap.toList feats
          feats   = scaling $ evalState (toFeatureSet fs) (featureMap . modelData $ m)
          scaling = if I.scale (settings . modelData $ m) then scaleFeatureSet (scalingMap . modelData $ m) else id
          origClass k  = case IntMap.lookup k (inverseClassMap . modelData $ m) of { Just k' -> k' }

classify m = fst . head . distribution m 

instance (Ord a, B.Binary a) => B.Binary (Table a) where
    put (T i m) = B.put i >> B.put m
    get = liftM2 T B.get B.get

instance (Ord lab, Ord key, Ord sym,
          B.Binary lab, 
          B.Binary key, 
          B.Binary sym, 
          B.Binary num) => B.Binary (ModelData lab key sym num) where
    put (ModelData cm fm sm s icm) = do
      B.put cm  
      B.put fm 
      B.put sm 
      B.put s  
      B.put icm
    get = return ModelData
          `ap` B.get
          `ap` B.get
          `ap` B.get 
          `ap` B.get
          `ap` B.get

instance (Ord lab, Ord key, Ord sym,
          B.Binary lab, 
          B.Binary key, 
          B.Binary sym, 
          B.Binary num) => B.Binary (Model lab key sym num) where
    put (Model x y) = B.put x >> B.put y
    get = liftM2 Model B.get B.get

tempprefix = "GramLab.Maxent.ZhangLe.Model"


toByteString m = do
  mb <- I.toByteString (model m)
  return $ B.encode (mb,(modelData m))

fromByteString s = do
  let (mb,md) = B.decode s
  m <- I.fromByteString mb
  return $ Model { model = m , modelData = md }
  

save path m = toByteString m >>= BS.writeFile path
load path = BS.readFile path >>= fromByteString
  
