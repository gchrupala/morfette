{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns , FlexibleContexts #-}
module GramLab.Perceptron.Multiclass 
    ( Model
    , bounds
    , train
    , learn
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
import Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Bits
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

{-# INLINE decode' #-}
decode' :: (Float, DenseVector (Y,I), DenseVector (Y,I)) 
           -> [Y]
           -> X
           -> Y
decode' w ys x = snd . maximum 
                       $ [ (w`dot'`phi x y,y) | y <- ys ]  


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

checker w x = True
type Key = String
learn1 :: Float 
      -> ((Y,I),(Y,I))
      -> Int
      -> [Key]
      -> [(X,Y)]
      -> Model
learn1 rate bounds@((lo,_),(hi,_)) span ws xys = 
      MC 
    . runSTUArray 
    $ do 
                                                    -- initialization --
      m@(c,params,params_a) <- liftM3 (,,) (newSTRef 1)
                                           (newArray bounds 0)
                                           (newArray bounds 0)
      rsref <- newSTRef ([]::[Bool])
      for_ (zip3 [1..] ws xys) $ \(i,w,(x,y)) -> do   -- update loop --
        let ys = filter (checker w) [ lo .. hi ]
        r <- update rate m ys x y 

{-        params' <- unsafeFreeze params
        let y'= decode (MC params') ys x
            phi_xy = phi x y
            phi_xy' = phi x y'
        when (y' /= y) $ do 
          params `plus_` (phi_xy `scale` rate)
          params `plus_` (phi_xy' `scale` (rate * (-1)))
          c' <- readSTRef c
          params_a `plus_` (phi_xy `scale` (rate * fromIntegral c'))
          params_a `plus_` (phi_xy' `scale` (rate * (-1) * fromIntegral c'))
        modifySTRef c (+1)
        let r = y' /= y
-}
        modifySTRef rsref (take span . (r:))
        when (i`rem`span == 0) $ do
          rs <- readSTRef rsref
          let err :: Double
              err = (fromIntegral . sum . map fromEnum) rs 
                    / fromIntegral (length rs)
          runLogger $ hPutStrLn stderr
                    $ printf "Step %d, example %d: error: %2.4f" 
                             (i`div`span) i err
                                                    -- termination --
      finalParams (c, params,  params_a)
      return params

learn :: Float 
      -> ((Y,I),(Y,I))
      -> Int
      -> [Key]
      -> [(X,Y)]
      -> Model
learn rate bounds@((lo,_),(hi,_)) span ws xys = MC m  
  where m = runSTUArray $ do
              trace (show bounds) () `seq` return ()
              params <- newArray bounds 0
              params_a <- newArray bounds 0
              c <- newSTRef 1
              let ys = [lo..hi]
                  ixys = zip [1..] xys
              eref <- newSTRef (0::Int)
              for_ ixys $ \ (i,(x,y)) -> do
                 params' <- unsafeFreeze params
                 let ys' = ys -- [ y | y <- ys , yss A.! (i,y) ] 
                     y'= decode (MC params') ys' x
                     phi_xy = phi x y
                     phi_xy' = phi x y'
                 when (y' /= y) $ do
                        modifySTRef eref (+1)
                        params `plus_` (phi_xy `scale` rate)
                        params `plus_` (phi_xy' `scale` (rate * (-1)))
                        c' <- readSTRef c
                        params_a `plus_` (phi_xy `scale` (rate * fromIntegral c'))
                        params_a `plus_` (phi_xy' `scale` (rate * (-1) * fromIntegral c'))
             
                 when (i`rem`span == 0) $ do
                     e <- readSTRef eref
                     writeSTRef eref 0
                     let err :: Double
                         err = fromIntegral e / fromIntegral span
                     unsafeIOToST $ hPutStrLn stderr
                                  $ printf "Step %d, example %d: error: %2.4f" 
                                      (i`div`span) i err
                 modifySTRef c (+1)
              finalParams (c, params,  params_a)
              return params
update :: Float 
       -> (STRef s Int, DenseVectorST s (Y,I), DenseVectorST s (Y,I))
       -> [Y] 
       -> X
       -> Y
       -> ST s Bool
update rate (c,params,params_a) ys x y = do
      params' <- unsafeFreeze params
      let y'= decode (MC params') ys x
          phi_xy = phi x y
          phi_xy' = phi x y'
      when (y' /= y) $ do 
        params `plus_` (phi_xy `scale` rate)
        params `plus_` (phi_xy' `scale` (rate * (-1)))
        c' <- readSTRef c
        params_a `plus_` (phi_xy `scale` (rate * fromIntegral c'))
        params_a `plus_` (phi_xy' `scale` (rate * (-1) * fromIntegral c'))
      modifySTRef c (+1)
      let !r = y' /= y
      return r
iter ::    Float
        -> [Y] --A.UArray (Int,Int) Bool
        -> [(X, Y)]
        -> (STRef s Int, DenseVectorST s (Y,I), DenseVectorST s (Y,I))
        -> ST s ()
iter rate ys ss (c,params,params_a) = do
    for_ (zip [0..] ss) $ \ (i,(x,y)) -> do
      params' <- unsafeFreeze params
      let ys' = ys -- [ y | y <- ys , yss A.! (i,y) ] 
          y'= decode (MC params') ys' x
          phi_xy = phi x y
          phi_xy' = phi x y'
      when (y' /= y) $ do 
        params `plus_` (phi_xy `scale` rate)
        params `plus_` (phi_xy' `scale` (rate * (-1)))
        c' <- readSTRef c
        params_a `plus_` (phi_xy `scale` (rate * fromIntegral c'))
        params_a `plus_` (phi_xy' `scale` (rate * (-1) * fromIntegral c'))
      modifySTRef c (+1)

train ::    Float
         -> Int
         -> ((Y,I), (Y,I))
         -> A.UArray (Int,Int) Bool
         -> [(X, Y)]
         -> Model
train rate epochs bounds yss xys =  MC m
  where m = runSTUArray $ do
              trace (show bounds) () `seq` return ()
              params <- newArray bounds 0
              params_a <- newArray bounds 0
              c <- newSTRef 1
              let ixys = zip [0..] xys
                  ((_,lo),(_,hi)) = A.bounds yss
                  ys = [lo..hi]
              for_ [1..epochs] $ 
                     \i -> do iter rate {- yss -} ys xys (c,params,params_a)
                              c' <- readSTRef c
                              params' <- unsafeFreeze params
                              params_a' <- unsafeFreeze params_a
                              let w  = (fromIntegral c',params',params_a')
                                  corr = sum 
                                         . map (\(j,(x,y)) -> 
                                                let s = [ y | y <- ys 
                                                          , yss A.!(j,y)]
                                                in fromEnum 
                                                       $  y /= decode' w s x)
                                         $ ixys
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
