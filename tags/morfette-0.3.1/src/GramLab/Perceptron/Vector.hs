{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module GramLab.Perceptron.Vector 
    ( SparseVector
    , DenseVector
    , DenseVectorST
    , for_
    , plus_
    , scale
    , dot 
    , dot'
    , unsafeDot
    )
where

import Data.Array.ST
import Data.Array.Unboxed (UArray,bounds,(!))
import Control.Monad.ST
import Data.STRef
import GHC.Arr (unsafeIndex)
import Data.Array.Base (unsafeAt)

type SparseVector y i = ([(i,Float)],y)
type DenseVectorST s i = STUArray s i Float
type DenseVector i = UArray i Float


{-# INLINE for_ #-}
for_ xs f = mapM_ f xs


{-# SPECIALIZE plus_ :: DenseVectorST s (Int,Int) 
                     -> SparseVector Int Int -> ST s () #-}
plus_ :: (Show (y,i),Ix (y,i)) => 
         DenseVectorST s (y,i) 
      -> SparseVector y i -> ST s ()
plus_ w (v,!y) = do
  for_ v $ \(!i,!vi) -> do
             !wi <- readArray w (y,i) 
             writeArray w (y,i) (wi + vi)

{-# SPECIALIZE scale :: SparseVector Int Int 
                     -> Float 
                     -> SparseVector Int Int #-}
scale :: (Ix i)  => SparseVector y i -> Float -> SparseVector y i
scale (v,y) n = (map (\(i,vi) -> (i,vi*n)) v,y)

{-# INLINE dot #-}
{-# SPECIALIZE dot :: DenseVector (Int,Int) 
                   -> ([(Int,Float)],Int)-> Float #-}
dot :: (Ix (y,i)) => DenseVector (y,i) -> ([(i,Float)],y) -> Float
dot w (x,!y) = go 0 x
    where go !s [] = s
          go !s ((!i,!xi):x) = go (s + (w ! (y,i)) * xi) x

{-# INLINE dot' #-}
dot' :: (Float,DenseVector (Int,Int),DenseVector (Int,Int)) 
     -> ([(Int,Float)],Int)
     -> Float
dot' (!c,params,params_a) (x,!y) = go 0 x
  where go !s [] = s
        go !s ((!i,!xi):x) = 
            let e   = params   ! (y,i)
                e_a = params_a ! (y,i)
            in go (s + (e - (e_a * (1/c))) * xi) x

{-# INLINE unsafeDot #-}
{-# SPECIALIZE unsafeDot :: DenseVector (Int,Int) 
                         -> ([(Int,Float)],Int)-> Float #-}
unsafeDot :: (Ix (y,i)) => DenseVector (y,i) -> ([(i,Float)],y) -> Float
unsafeDot w (x,!y) = go 0 x
    where bs = bounds w
          go !s [] = s
          go !s ((!i,!xi):x) = go (s + unsafeAt w (unsafeIndex bs (y,i)) * xi) x

