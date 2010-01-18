{-# OPTIONS -fno-monomorphism-restriction #-}

module GramLab.Metrics ( Dist  
                       , sum
                       , toDist
                       , distFromMultiSet
                       , dotProduct
                       , entropy
                       , mutualInformation
                       , infoGain
                       , gainRatio
                       , euclidean
                       , squaredEuclidean
                       , elk
                       , crossEntropy
                       , kullbackLeibler 
                       , jensenShannon
                       , cosine
                       , l1_norm
                       , infoRad
                       , rer
                       )
where
import Prelude hiding (sum)
import Control.Monad (ap)
import qualified Data.Map as Map
import qualified  Data.MultiSet as MS 
import qualified Data.Foldable as F
import qualified Data.List as List
type Dist a = Map.Map a Double

--------------- Error metrics ---------------------------------------------------- 
-- | Relative error reduction
rer hi lo = ((1-lo)-(1-hi))/(1-lo)
--------------- Miscellaneous ----------------------------------------------------
----------------------------------------------------------------------------------
dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct  = (sum .) . zipWith (*)

euclideanDistance :: (Floating b) => [b] -> [b] -> b
euclideanDistance = (sqrt .) . squaredEuclideanDistance

squaredEuclideanDistance :: (Num b) => [b] -> [b] -> b
squaredEuclideanDistance = (sum .) . zipWith (\ pi qi -> (pi-qi)^2)

entropy :: (Ord a) => Dist a -> Double
entropy  = liftToDist $ negate . sum . fmap (\px -> if px == 0 then 0 else px * logBase 2 px) 

-- | Mutual information of two discrete random variables X and X is defined as:
--   I(X;Y) = SUM_y SUM_x p(x,y) log (p(x,y)/p(x)p(y))
--   I(X;Y) = H(X) - H(X|Y)
--          = H(Y) - H(Y|X)
--          = H(X) + H(Y) - H(X,Y)
mutualInformation :: (Ord a, Ord b) => [(a,b)] -> Double
mutualInformation xys = sum [f x y | (x,y) <- Map.keys dxy ]
    where f x y = let pxy = p_xy (x,y) 
                  in if pxy == 0 then 0 else pxy * logBase 2 (pxy / (p_x x * p_y y))
          p_x   = prob $ toDist xs
          p_y   = prob $ toDist ys
          dxy   = toDist $ zip xs ys
          p_xy  = prob dxy
          (xs,ys) = unzip xys

-- | Information gain. Information gain about a random variable X
-- | IG(C,V) = H(C) - SUM_{v in V} p(V)H(C|V)
-- | (a : class, b : feature value)
infoGain :: (Ord a, Ord b) => [(a,b)] -> Double
infoGain xys = entropy dist_x - sum [f y | y <- y_vals]
    where f y =  prob dist_y y * entropy given_y
              where given_y = distFromMultiSet (MS.filter ((==y) . snd) xyset)
          xyset     = MS.fromList xys
          (xs,ys)   = unzip xys
          dist_x    = toDist xs
          dist_y    = toDist ys
          y_vals    = Map.keys dist_y
          yset      = MS.fromList ys
          size      = fromIntegral . MS.size


-- | (a : class, b : feature value)
gainRatio :: (Ord a, Ord b) => [(a,b)] -> Double
gainRatio xys = infoGain xys / entropy (toDist . snd . unzip $ xys)

    
--------------- Comparing distributions ------------------------------------------
----------------------------------------------------------------------------------
euclidean = liftToDist2 euclideanDistance
squaredEuclidean = liftToDist2 squaredEuclideanDistance
-- | Expected likelihood kernel - dot product of two probability distributions
elk :: (Ord a) => Dist a -> Dist a -> Double
elk = liftToDist2 dotProduct

crossEntropy :: (Ord a) => Dist a -> Dist a -> Double
crossEntropy = liftToDist2 f
    where f p q = negate $ sum $ zipWith (\px xq -> if px == 0 then 0 else px * logBase 2 xq) p q
-- | Kullback-Leibler divergence (also information divergence, information gain, or relative entropy)
--   D(P||Q) = SUM_i P(i) log_2 (P(i)/Q(i))
kullbackLeibler :: (Ord a) => Dist a -> Dist a -> Double
kullbackLeibler = liftToDist2 kl
kl p q = sum $ zipWith (\px xq -> if px == 0 then 0 else px * logBase 2 (px/xq)) p q

jensenShannon :: (Ord a) => Dist a -> Dist a -> Double
jensenShannon = liftToDist2 f
    where f p q = sum $ zipWith (\px qx -> if px == 0 then 0  else px * (logBase 2 (px / ((px/2) + (qx/2))))) p q
-- | Cosine for distributions
cosine :: (Ord a) => Dist a -> Dist a -> Double
cosine = liftToDist2 f
    where f p q = sum $ zipWith (\px qx -> sqrt $ px * qx) p q
-- | L_1 norm 
l1_norm :: (Ord a) => Dist a -> Dist a -> Double
l1_norm = liftToDist2 f
    where f p q = sum $ zipWith (\px qx -> abs $ px - qx) p q
infoRad :: (Ord a) => Dist a -> Dist a -> Double
infoRad = liftToDist2 f
    where f p q = kl p combined + kl p combined
              where combined = zipWith (\px qx -> (px+qx)/2) p q
-- | Convert a function defined on a list to one defined on a Dist(ribution)
liftToDist f = f . Map.elems
-- | Convert a function defined on two lists to one defined on two Dist(ributions)
liftToDist2 f p q = f (Map.elems p') (Map.elems q')
    where (p',q') = p `padDist` q

toDist :: (Ord a) => [a] -> Dist a
toDist  = distFromMultiSet . MS.fromList


distFromMultiSet set = Map.map (\e -> fromIntegral e / fromIntegral c) m'
    where c  = MS.size set
          m' = MS.toMap set

prob :: (Ord a) => Dist a -> a -> Double
prob = flip (Map.findWithDefault 0)

padDist d d' =   ( Map.union d  (Map.map (const 0) d')
                 , Map.union d' (Map.map (const 0) d) )


-- non-strict sum causes stack overflow 
sum = List.foldl' (+) 0 

-- | BUT the following looks suspicious: 
{-
*GramLab.SimpleRerank.Utils> dotProduct [0.25,0.25,0.5][0.25,0.25,0.5]
0.375
*GramLab.SimpleRerank.Utils> dotProduct [0.25,0.25,0.5][0.5,0.0,0.5]
0.375
-}