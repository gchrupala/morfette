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
import Debug.Trace 

type Logger x y = Int -> ([y] -> x -> y) -> String
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

train ::    Logger X Y 
         -> Int 
         -> Double
         -> Float
         -> Int
         -> ((Y,I), (Y,I))
         -> [[Y]]
         -> [(X, Y)]
         -> Model
train logger th1 th2 rate epochs bounds yss ss =  MC m
  where m = runSTUArray $ do
              trace (show bounds) () `seq` return ()
              let ((lo,_),(hi,_)) = bounds
                  ys = [lo..hi]
              params <- newArray bounds 0
              params_a <- newArray bounds 0
              c <- newSTRef 1
              for_ [1..epochs] $ 
                       \i -> do iter rate yss ss (c,params,params_a)
                                ps <- finalParams (c,params,params_a)
                                ps' <- unsafeFreeze ps
                                runLogger 
                                 $ hPutStrLn stderr (logger i $ decode (MC ps'))
              final <- finalParams (c, params,  params_a)
              return final

runLogger f = unsafeIOToST f

finalParams :: (STRef s Int, DenseVectorST s (Y,I), DenseVectorST s (Y,I))
            -> ST s (DenseVectorST s (Y,I))
finalParams (c,params,params_a) = do
  (l,u) <- getBounds params
  out <- newArray (l,u) 0
  c' <- fmap fromIntegral (readSTRef c)
  for_ (range (l,u)) $ \i -> do
      e   <- readArray params   i
      e_a <- readArray params_a i
      writeArray out i (e - (e_a * (1/c')))
  return out



m `at` i = Map.findWithDefault 0 i m
        
counts_x :: [(X,Y)] -> (Map.Map (I,Y) Int
                       ,Map.Map I Int
                       ,Map.Map Y Int)
counts_x xys = foldl' f (Map.empty,Map.empty,Map.empty) xys
    where f (!cxy,!cx,!cy) (!x,!y) = 
              ( foldl' (\z (i,_) -> Map.insertWith' (+) (i,y) 1 z) cxy x
              , foldl' (\z (i,_) -> Map.insertWith' (+) i 1 z) cx x
              , Map.insertWith' (+) y 1 cy )

sum :: [Double] -> Double                                    
sum = foldl' (+) 0
product = foldl' (*) 1

entropyRanking :: Int -> [(X,Y)] -> [(I,Double,[Y])]
entropyRanking freqth xys = 
    let (!cxy,!cx,!cy) = counts_x xys
        n =  sum . map fromIntegral . Map.elems $ cy 
        ys_x x = map (\((_,y),c) -> (y,c))
               . filter (\((x',_),c) -> x == x') 
               . Map.toList 
               $ cxy
    in [ let ys = ys_x x in (x,entropy . map (fromIntegral . snd) 
                                  $ ys,map fst ys) 
                | x <-    Map.keys 
                        . Map.filter (>freqth)
                        $ cx ]

entropy :: [Double] -> Double
entropy xs = 
    let n = sum xs
    in negate . sum $ [ if x == 0 then 0 else x/n * logBase 2 (x/n) | x <- xs ]

makeFeatDict :: Int -> Double -> [(X, Y)] -> Map.Map I [Y]
makeFeatDict th1 th2 = 
    if th2 <= 0.0 
    then const Map.empty 
    else Map.fromList 
             . map (\r@(x,s,ys) -> (x,ys))
             . filter (\(x,s,ys) -> s < th2)
             . entropyRanking th1
             
