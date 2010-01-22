module GramLab.Binomial (binomialTest)
where
import qualified Data.List as List
import Debug.Trace
import Test.QuickCheck
-- | Calculates the two-sided p-value for n trials and c successes with probability 0.5
binomialTest n c | c > n = error "GramLab.Binomial.binomialTest c greater than n"
binomialTest n c = 1 `min` (2 * if c >= n `div` 2 
                                then binomialTestGreater n 0.5 c 
                                else binomialTestGreater n 0.5 (n-c))
-- | Calculates the one-sided p-value for n trials and c successes with probability p
binomialTestGreater :: Integer -> Double -> Integer -> Double
binomialTestGreater n p c = sum $ fmap (\(pr1,pr2,pw1,pw2) -> fromIntegral (pr1 `div` pr2) * pw1 * pw2) $
                     List.zip4 (products1 n c)
                               (products2 n c)
                               (powers1 p n c)
                               (powers2 p n c)
divv a b | trace (show (a,b)) False = undefined
divv a b = a `div` b 

-- | 
spec_products1 n c = fmap (\x -> product [x+1..n]) [c..n]
--products1 :: Int -> Int -> [Int]
products1 n c | c   == n    = [1]
              | c+1 == n    = [n,1] --
              | otherwise = let x:xs = products1 n (c+1) in ((1+c)*x):x:xs

-- | 
spec_products2 n c = fmap (\x -> product [1..n-x]) [c..n]
--products2 :: Int -> Int -> [Int]
products2 n c | c == n    = [1]
              | otherwise = let x:xs = products2 n (c+1) in ((n-c)*x):x:xs

-- | 
spec_powers1 p n c = fmap (p^) [c..n]
--powers1 :: Double -> Int -> Int -> [Double]
powers1 p n c = reverse (powers1' [p^c] p n c)
powers1' (x:xs) p n c | n == c    = x:xs
                     | otherwise = powers1' ((p*x):x:xs) p n (c+1)

-- | 
spec_powers2 p n c =  fmap (\x -> (1-p)^(n-x)) [c..n]
--powers2 :: Double -> Int -> Int -> [Double]
powers2 p n c | n == c     = [1]
              | otherwise  = let x:xs = powers2 p n (c+1) in (1-p)*x:x:xs

prop_products1 :: (Integer,Integer) -> Property
prop_products1 (n,c) = n > 0 && c > 0 && n >= c ==> spec_products1 n c == products1 n c

prop_products2 :: (Integer,Integer) -> Property
prop_products2 (n,c) = n > 0 && c > 0 && n >= c ==> spec_products2 n c == products2 n c

epsilon = 1.0e-15

prop_powers1 :: (Double,Integer,Integer) -> Property
prop_powers1 (p,n,c) = p >= 0 && p <= 1 && n > 0 && c > 0 && n >= c 
                         ==> all (<epsilon) (zipWith (\a b -> abs (a-b)) (spec_powers1 p n c) (powers1 p n c))
prop_powers2 :: (Double,Integer,Integer) -> Property
prop_powers2 (p,n,c) = p >= 0 && p <= 1 && n > 0 && c > 0 && n >= c 
                         ==> all (<epsilon) (zipWith (\a b -> abs (a-b)) (spec_powers2 p n c) (powers2 p n c))
