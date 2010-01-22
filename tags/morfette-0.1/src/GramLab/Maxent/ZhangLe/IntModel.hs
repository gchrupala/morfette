{-# OPTIONS_GHC -fno-monomorphism-restriction #-}
{-# INCLUDE c_maxentmodel.h #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module GramLab.Maxent.ZhangLe.IntModel ( IntModel
                                       , train
                                       , evalAll
                                       , load
                                       , save
                                       , toByteString
                                       , fromByteString
                                       , TrainSettings(..)
                                       , trainSettings
                                       )

where

import Foreign.C 
import Foreign
import ForeignPtr
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Binary.Strict as B
import Control.Exception (bracket)
import Control.Monad
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS
import GramLab.Maxent.TempDir
import GramLab.Utils (padRight)


data ModelH = ModelH

foreign import ccall unsafe c_train_C_ZhangLeModel ::
           CInt
        -> CDouble
        -> CDouble
        -> CInt
        -> Ptr CInt
        -> Ptr CInt
        -> Ptr (Ptr CInt)
        -> Ptr (Ptr CFloat)
        -> CInt
        -> IO (Ptr ModelH)
foreign import ccall unsafe "&c_destroy_C_ZhangLeModel" c_destroy_C_ZhangLeModel ::
    FunPtr(Ptr ModelH -> IO ())
foreign import ccall unsafe c_eval_all ::
    Ptr ModelH 
 -> Ptr CInt
 -> Ptr CFloat
 -> CInt
 -> Ptr CInt
 -> Ptr CDouble
 -> CInt
 -> IO ()
foreign import ccall unsafe c_save :: Ptr ModelH -> CString -> IO ()
foreign import ccall unsafe c_load :: CString -> IO (Ptr ModelH)

-- put this in hi level interface?
data TrainSettings = TrainSettings { iter      :: Int
                                   , gaussian  :: Double
                                   , tolerance :: Double 
                                   , verbose   :: Bool 
                                   , scale     :: Bool } deriving (Show,Read)
instance B.Binary TrainSettings where
    put (TrainSettings it g t v s) = B.put it >> B.put g >> B.put t >> B.put v >> B.put s
    get = liftM5 TrainSettings B.get B.get B.get B.get B.get
trainSettings = TrainSettings 30 0 1e-05 True False

data IntModel = IntModel  { modelLabels   :: IntSet.IntSet 
                          , modelFeatures :: IntSet.IntSet
                          , modelPtr      :: ForeignPtr ModelH  }

train :: TrainSettings -> [(Int,[(Int,Double)])] -> IO IntModel
train s examples = do 
  bracket (do liftM2 (,)   (mapM newArray featids)
                           (mapM newArray featvals))
          (\(pl1,pl2) -> mapM_ free pl1 >> mapM_ free pl2)
          (\(pl1,pl2) -> withArrayLen pl1 $ \size featIdPtr ->
                               withArray pl2 $ \featValPtr ->
                                   withArray labels $ \labelPtr ->
                                       withArray rowsizes $ \rowSizePtr ->
                                 do doTraining size labelPtr rowSizePtr featIdPtr featValPtr)
 where labels   =  map (int . fst)             examples
       rowsizes =  map (int . length . snd)    examples
       featids  =  map (map (int . fst) . snd) examples
       featvals =  map (map (dbl . snd) . snd) examples
       doTraining csize labelPtr rowSizePtr featIdPtr featValPtr = do
                     m <- c_train_C_ZhangLeModel (int $ iter s) 
                                                 (dbl $ gaussian s) 
                                                 (dbl $ tolerance s) 
                                                 (int . fromEnum $ verbose s)
                                                 labelPtr
                                                 rowSizePtr
                                                 featIdPtr
                                                 featValPtr
                                                 (int csize) 
                     fp <- newForeignPtr c_destroy_C_ZhangLeModel m
                     return $ IntModel { modelLabels   = IntSet.fromList (map fst examples)
                                       , modelFeatures = IntSet.fromList (map fst . snd =<< examples)
                                       , modelPtr      = fp }
evalAll :: IntModel -> [(Int,Double)] -> [(Int,Double)]
evalAll m fs = unsafePerformIO $ do
     withForeignPtr (modelPtr m) $ \p -> do 
          withArrayLen (map (int . fst) fs) $ \featSize featPtr -> 
              withArray (map (dbl . snd) fs) $ \realPtr ->                                            
                  allocaArray labSize $ \labPtr -> 
                      allocaArray labSize $ \probPtr ->  do
                                           c_eval_all p featPtr realPtr (int featSize) labPtr probPtr (int labSize)
                                           labels <- peekArray labSize labPtr
                                           probs  <- peekArray labSize probPtr
                                           return $ zipWith (\l p -> (int l, dbl p)) labels probs
    where labSize = IntSet.size . modelLabels $ m
                     
saveModelPtr :: FilePath -> ForeignPtr ModelH -> IO ()
saveModelPtr path m = 
    withForeignPtr m $ \p ->
        withCString path $ \cpath ->
            c_save p cpath
loadModelPtr :: FilePath -> IO (ForeignPtr ModelH)
loadModelPtr path =
    withCString path $ \cpath -> do
        m <- c_load cpath
        newForeignPtr c_destroy_C_ZhangLeModel m

tempprefix = "GramLab.Maxent.ZhangLe.IntModel"

toByteString :: IntModel -> IO BS.ByteString
toByteString m = do
  bits <- withTempFilePath tempprefix $ \path -> do saveModelPtr path (modelPtr m)
                                                    BS.readFile path
  return $ B.encode (modelLabels m, modelFeatures m, bits)

fromByteString :: BS.ByteString -> IO IntModel 
fromByteString s = do
  let (ls,fs,bits) = B.decode s
  m <- withTempFilePath tempprefix $ \path -> do BS.writeFile path bits
                                                 loadModelPtr path
  return $ IntModel { modelLabels = ls , modelFeatures = fs , modelPtr = m  }

toByteStringPure :: IntModel -> BS.ByteString
toByteStringPure m = unsafePerformIO (toByteString m)
{- NOINLINE toByteStringPure -}
fromByteStringPure :: BS.ByteString ->  IntModel 
fromByteStringPure s = unsafePerformIO (fromByteString s)
{- NOINLINE fromByteStringPure -}

instance B.Binary IntModel where
    put m  = B.put (toByteStringPure m)
    get  = do s <- B.get
              return (fromByteStringPure s)
                              
save :: FilePath -> IntModel -> IO ()
save path m = toByteString m >>= BS.writeFile path 

load :: FilePath -> IO IntModel
load path = BS.readFile path >>= fromByteString
  


-- decrease conversion clutter
int = fromIntegral
dbl = realToFrac

