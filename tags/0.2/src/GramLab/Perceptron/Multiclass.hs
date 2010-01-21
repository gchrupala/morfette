{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns , FlexibleContexts #-}
module GramLab.Perceptron.Multiclass 
    ( Model
    , bounds
    , train
    , decode
    , distribution
    )
where

import Data.Array.ST
import qualified Data.Array.Unboxed as A
import Control.Monad.ST
import Data.STRef
import Control.Monad
import GramLab.Perceptron.Vector
import System.IO
import Data.List (foldl',sort)
import Prelude hiding (sum,product)
import qualified Data.Binary as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))
import Data.Maybe (isJust,fromMaybe)
import GramLab.Utils (uniq)
import Text.Printf (printf)
import Debug.Trace 

newtype Model = MC { weights :: DenseVector (Y,I)
                   } deriving (Eq,Show)

bounds = A.bounds . weights

instance B.Binary Model where
    put (MC m) = do
        let (lo,hi) = A.bounds m
            xs = filter (\(_,e) -> e /= 0.0) . A.assocs $ m
        B.put (lo,hi)
        B.put xs
    get = do
      (lo,hi) <- B.get
      xs <- B.get
      xs == xs `seq` return ()
      return $ MC (A.accumArray (+) 0 (lo,hi) $ xs)

type Y = Int
type X = [(I,Float)]
type I = Int

{-# INLINE phi #-}
phi :: X -> Y -> (X,Y)
phi x y = (x,y)

{-# INLINE decode #-}
decode :: Model -> [Y] -> X -> Y
decode (MC w) ys x = snd . maximum 
                       $ [ (w`dot`phi x y,y) | y <- ys ]

{-# INLINE decode_ #-}
decode_ :: (STRef s Int, DenseVectorST s (Y,I), DenseVectorST s (Y,I)) 
           -> [Y]
           -> X
           -> ST s Y
decode_ w ys x =   fmap (snd . maximum)  
                 $ mapM (\y -> do { r <- w`dot_`phi x y ; return (r,y) } )
                 $ ys


{-# INLINE softmax #-}
{-# SPECIALIZE softmax :: [Float] -> [Float] #-}
softmax x = 
    let !x_max = maximum x
        !a = foldl' (+) 0  . map (\ !x_i -> exp $ x_i - x_max)  $ x
    in  [ exp $ x_i - x_max - log a | !x_i <- x ]

{-# INLINE distribution #-}
distribution ::  Model -> [Y] -> X -> [(Y, Float)]
distribution (MC w) ys x = 
    let swap (!x,!y) = (y,x)
        fxs = map ((w`dot`) . phi x) ys
    in reverse . map swap . sort $ zip (softmax fxs) ys

iter ::    Float
        -> [[Y]]
        -> [(X, Y)]
        -> (STRef s Int, DenseVectorST s (Y,I), DenseVectorST s (Y,I))
        -> ST s ()
iter rate yss ss (c,params,params_a) = do
    for_ (zip yss ss) $ \ (ys',(x,y)) -> do
      params' <- unsafeFreeze params
      let y'= decode (MC params') ys' x
          phi_xy = phi x y
          phi_xy' = phi x y'
      when (y' /= y) $ do 
        params `plus_` (phi_xy `scale` rate)
        params `plus_` (phi_xy' `scale` (rate * (-1)))
        c' <- readSTRef c
        params_a `plus_` (phi_xy `scale` (rate * fromIntegral c'))
        params_a `plus_` (phi_xy' `scale` (rate * (-1) * fromIntegral c'))
      modifySTRef c (+1)

train ::    Int 
         -> Double
         -> Float
         -> Int
         -> ((Y,I), (Y,I))
         -> [[Y]]
         -> [(X, Y)]
         -> Model
train th1 th2 rate epochs bounds yss xys =  MC m
  where m = runSTUArray $ do
              trace (show bounds) () `seq` return ()
              params <- newArray bounds 0
              params_a <- newArray bounds 0
              c <- newSTRef 1
              for_ [1..epochs] $ 
                       \i -> do iter rate yss xys (c,params,params_a)
                                corr <- fmap sum 
                                        . flip mapM (zip yss xys)
                                        $ \(ys,(x,y)) -> do 
                                          y'<- decode_ (c,params,params_a) ys x 
                                          return . fromEnum $ y' /= y
                                let err :: Double
                                    err = fromIntegral corr / 
                                          fromIntegral (length xys)
                                runLogger 
                                  $ hPutStrLn stderr
                                  $ printf "Iteration %d: error: %2.4f" i err 
              finalParams (c, params,  params_a)
              return params

finalParams :: (STRef s Int, DenseVectorST s (Y,I), DenseVectorST s (Y,I))
            -> ST s ()
finalParams (c,params,params_a) = do
  (l,u) <- getBounds params
  c' <- fmap fromIntegral (readSTRef c)
  for_ (range (l,u)) $ \i -> do
      e   <- readArray params   i
      e_a <- readArray params_a i
      writeArray params i (e - (e_a * (1/c')))

{-# NOINLINE runLogger #-}
runLogger f = unsafeIOToST f

m `at` i = Map.findWithDefault 0 i m
        
counts_x :: [(X,Y)] -> (Map.Map (I,Y) Int
                       ,Map.Map I Int
                       ,Map.Map Y Int)
counts_x xys = foldl' f (Map.empty,Map.empty,Map.empty) xys
    where f (!cxy,!cx,!cy) (!x,!y) = 
              ( foldl' (\z (i,_) -> Map.insertWith' (+) (i,y) 1 z) cxy x
              , foldl' (\z (i,_) -> Map.insertWith' (+) i 1 z) cx x
              , Map.insertWith' (+) y 1 cy )

sum :: (Num n) => [n] -> n
{-# SPECIALIZE INLINE sum :: [Double] -> Double  #-}
{-# SPECIALIZE INLINE sum :: [Float] -> Float  #-}
sum = foldl' (+) 0
