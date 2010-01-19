{-# LANGUAGE FlexibleContexts , BangPatterns #-}
module GramLab.Perceptron.Model ( train
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
import qualified GramLab.Perceptron.IntModel as I
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as B
import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
import Data.Map ((!))
import Data.List (foldl')
import Data.Maybe (catMaybes)

import Data.Char (isAlphaNum)
import GramLab.Intern
import GramLab.Data.Assoc
import GramLab.FeatureSet
import System.IO
import Debug.Trace

type Example label features = (label,features)
data ModelData lab key sym num = 
    ModelData { featureMap :: Table (key,Maybe sym) 
              , settings   :: I.TrainSettings 
              , inverseClassMap   :: IntMap.IntMap lab
              } deriving (Eq,Show)
data Model lab key sym num = Model { model      :: I.IntModel
                                   , modelData  :: ModelData lab key sym num }
                           deriving (Eq,Show)

invertMap = Map.foldWithKey (\k v m' -> IntMap.insert v k m') IntMap.empty
maxValues = IntMap.unionsWith max

train p examples = Model { model       = m 
                         , modelData   = 
                             ModelData { featureMap  = fm 
                                       , settings    = p   
                                       , inverseClassMap   = 
                                         invertMap $ let (T _ icm) = cm   in icm
                                       }}
  where m = I.train p samples
        (ks,xs)       = unzip 
                        . pruneSingletonFeats  
                        . pruneSingletonLabels
                        $ examples
        (labels,cm)   = runState (mapM intern ks)       initial
        (featsets,fm) = runState (mapM toFeatureSet xs) initial
        samples       = zipWith (\l fs -> (l,IntMap.toList fs)) 
                                labels 
                                featsets

pruneSingletonLabels :: (Ord y) => [(y,a)] -> [(y,a)]
pruneSingletonLabels yxs = 
    let counts = foldl' f Map.empty yxs
        f !z (!y,_) = Map.insertWith' (+) y 1 z
    in catMaybes [ if counts ! y > 1 then Just (y,x) else Nothing 
                       | (y,x) <- yxs ]


pruneSingletonFeats :: (Ord y) => 
                       [(y, [Feature String Double])] 
                    -> [(y, [Feature String Double])]
pruneSingletonFeats yxs =
    let counts = foldl' f Map.empty yxs
        f !cxy (_,!x) = 
            foldl' (\z i -> case i of
                              Null -> z
                              Num _ -> z
                              Sym s -> Map.insertWith' (+) s 1 z
                              Set ss -> 
                                foldl' (\z s -> Map.insertWith' (+) s 1 z)
                                       z
                                       ss)
                   cxy 
                   x
    in [ (y,[ case  f of
                Null ->  Null
                Num n -> Num n
                Sym s -> if counts ! s > 1 then Sym s else Null
                Set ss -> Set (filter (\s -> counts ! s > 1) ss)
                       | f <- x ]) 
         | (y,x) <- yxs ]

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
      format (l,fs) = unwords (show l 
                               : map (\(f,v) -> show f ++ ":" ++ show v) fs)
  mapM_ (hPutStrLn h . format) samples
  case dm of
    Write p -> BS.writeFile p (B.encode (cm',fm'))
    _       -> return ()


distribution m fs = fromAssoc $ map (\(k,v) -> (origClass k,v)) dist
    where dist    = I.evalAll (model m) s
          s       = IntMap.toList feats
          feats   = evalState (toFeatureSet fs) (featureMap . modelData $ m)
          origClass k  = 
              case IntMap.lookup k (inverseClassMap . modelData $ m) 
              of Just k' -> k' 
                 Nothing -> error 
                            $ "GramLab.Perceptron.Model.distribution: "
                             ++ "key not found: " ++ show k 

classify m = fst . head . distribution m 

instance (Ord a, B.Binary a) => B.Binary (Table a) where
    put (T i m) = B.put i >> B.put m
    get = liftM2 T B.get B.get

instance (Ord lab, Ord key, Ord sym,
          B.Binary lab, 
          B.Binary key, 
          B.Binary sym, 
          B.Binary num) => B.Binary (ModelData lab key sym num) where
    put (ModelData fm s icm) = do
      B.put fm 
      B.put s  
      B.put icm
    get = do 
      fm <- B.get
      fm == fm `seq` return ()
      s <- B.get 
      s == s `seq` return ()
      icm <- B.get
      icm == icm `seq` return ()
      return $ ModelData fm s icm

instance (Ord lab, Ord key, Ord sym,
          B.Binary lab, 
          B.Binary key, 
          B.Binary sym, 
          B.Binary num) => B.Binary (Model lab key sym num) where
    put (Model x y) = B.put x >> B.put y
    get = do 
      x <- B.get
      x == x `seq` return ()
      y <- B.get 
      y == y `seq` return ()
      return $ Model x y

save path m = BS.writeFile path (B.encode m)
load path = fmap B.decode $ BS.readFile path 
  
