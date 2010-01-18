{-# LANGUAGE BangPatterns #-}

module GramLab.Independence 
    ( chisq
    , mi
    , gr
    , counts
    , Counts
    )
where
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List (foldl')
import Prelude hiding (sum)

type Counts a b = ( Map.Map (a,b) Double
                  , Map.Map a     Double
                  , Map.Map b     Double )

counts :: (Ord a,Ord b) => [(a,b)] -> Counts a b
{-# SPECIALIZE counts :: [(Int,Int)] -> Counts Int Int #-}
counts xys = foldl' f (Map.empty,Map.empty,Map.empty) xys
    where f (!cxy,!cx,!cy) (!x,!y) = ( Map.insertWith' (+) (x,y) 1 cxy
                                     , Map.insertWith' (+) x 1 cx
                                     , Map.insertWith' (+) y 1 cy )

chisq :: (Ord a, Ord b) => Counts a b -> Double
{-# SPECIALIZE chisq :: Counts Int Int -> Double #-}
{-# SPECIALIZE chisq :: Counts Bool Bool -> Double #-}
chisq (cxy,cx,cy) = 
    let n = Map.fold (+) 0 cxy
        cell (x,y) nxy = 
            let nx = cx ! x
                ny = cy ! y
                exy = nx*ny/n
            in  (exy - nxy)^2 / exy
    in sum [ cell (x,y) nxy | ((x,y),nxy) <- Map.toList cxy ]

mi :: (Ord a, Ord b) => Counts a b -> Double
{-# SPECIALIZE mi :: Counts Int Int -> Double #-}
{-# SPECIALIZE mi :: Counts Bool Bool -> Double #-}
mi (cxy,cx,cy) = 
    let n = Map.fold (+) 0 cxy
        cell (x,y) nxy = 
            let nx = cx ! x
                ny = cy ! y
            in  nxy / n * logBase 2 (nxy * n / nx / ny)
    in sum [ cell (x,y) nxy | ((x,y),nxy) <- Map.toList cxy ]

gr :: (Ord a, Ord b) => Counts a b -> Double
{-# SPECIALIZE gr :: Counts Int Int -> Double #-}
{-# SPECIALIZE gr :: Counts Bool Bool -> Double #-}
gr cs@(cxy,cx,cy) = mi cs / entropy cx (Map.fold (+) 0 cxy)

entropy :: Map.Map k Double -> Double -> Double
entropy cx n = negate $ sum [ f nx | nx <- Map.elems cx ]
    where logn = logBase 2 n
          f nx = nx / n * (logBase 2 nx - logn)

{-# INLINE sum #-}
sum :: [Double] -> Double
sum = foldl' (+) 0

lrranking :: (Show x, Show y, Ord x, Ord y) => 
             [([(x, Double)], y)] 
          -> [(x, Double,[y])]
lrranking xys = 
    let (!cxy,!cx,!cy) = counts_x xys
        n =  sum . Map.elems $ cy 
        ys_x x = map snd 
               . filter (\(x',_) -> x == x') 
               . map fst 
               . Map.toList 
               $ cxy
    in [ (x,lr n cxy cy x, ys_x x) | x <-   Map.keys 
                                          . Map.filter (> 20)
                                          $ cx ]
lr :: (Ord x,Ord y) => 
                 Double
              -> Map.Map (x,y) Double 
              -> Map.Map y Double 
              -> x
              -> Double
lr n cxy cy x =
    let cxy_x = Map.filterWithKey (\(k,_) _ -> k == x) $ cxy
        ys = uniq . map snd . Map.keys $ cxy_x
        nx = sum . Map.elems $ cxy_x
    in (-2) * sum [ let r = cxy_x `at` (x,y) 
                    in if r == 0 
                       then 0 
                       else r * log (cy `at` y * nx / n / r) 
               | y <- ys ]