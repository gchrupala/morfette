import System
import System.Random
import GramLab.Statistics
{-
binomial' n c = factorial n `div` (factorial c * factorial (n - c))
factorial n = product [1..n]
-- | simplified to :
binomial n c = product [c'+1..n]
               `div`
               product [1..n-c']
    where c' | c > 2*n   = c
             | otherwise = n - c

binomP n p c = fromIntegral (binomial' n c) * p^c * (1-p)^(n-c)

-- FIXME the summations can be optimized a lot by dynamic programming
binomialTest EQ n p c = binomP n p c
binomialTest LT n p c = sum (fmap (binomP n p) [0..c])
binomialTest GT n p c = sum (fmap (binomP n p) [c..n])

--binomialTest' :: Int -> Double -> Int -> Double
binomialTest' n p c = sum $ fmap (\(pr1,pr2,pw1,pw2) -> fromIntegral (pr1 `div` pr2) * pw1 * pw2) $
                              List.zip4 (products1 n c)
                                        (products2 n c)
                          +              (powers1 p n c)
                                        (powers2 p n c)

-- | 
spec_products1 n c = fmap (\x -> product [x+1..n]) [c..n]
--products1 :: Int -> Int -> [Int]
products1 n c | c+1 == n    = [n,1] --
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

signTest xs ys = binomialTest GT n 0.5 c
    where trials    = List.filter (/=EQ) $ zipWith compare xs ys
          n         = length trials
          c         = length $ List.filter (==LT) trials
-}
shuffle rs xs ys = unzip (zipWith3 shuffleOne rs xs ys)
    where --shuffleOne n x y | trace (show (n,x,y)) False = undefined
          shuffleOne n x y | n > 0   = (x,y)
                           | otherwise = (y,x)
compareF xs ys = abs (f_score (sum xs) - f_score (sum ys)) -- taking abs means we do a two tailed check?
fscores xs ys = ((f_score (sum xs), f_score (sum ys))) -- taking abs means we do a two tailed check?

compareShuffling :: Double -> [Scores Int] -> [Scores Int] -> [Int] -> Int
compareShuffling d xs ys rs = if compareF xs' ys' >= d then 1 else 0
    where (xs',ys') = shuffle rs xs ys

randTest' :: (RandomGen g) =>
             Int -> g -> Int -> Double -> [Scores Int] -> [Scores Int] -> Int
randTest' i g count d xs ys 
    | i < 1     = count
    | otherwise = let (_,g') = next g 
                      rs     = randomRs (0,1) g 
                      count' = count + compareShuffling d xs ys rs
                  in randTest' (i-1) g' count' d xs ys

randTest :: (RandomGen g) => Int -> g -> [Scores Int] -> [Scores Int] -> Double
randTest i g xs ys = 
    let d     = compareF xs ys
        count = randTest' i g 0 d xs ys 
    in  p_val count i

p_val geq i = fromIntegral (geq+1) / fromIntegral (i+1)
        
    
runRandTest i file1 file2  = do
    text1 <- readFile file1
    text2 <- readFile file2
    g     <- newStdGen
    return $ randTest i g (readScores text1) (readScores text2)

readScores :: String -> [Scores Int]
readScores = fmap ((\[tp,t,g] -> Scores tp t g) . fmap read . words) . lines

main = do
  [file1,file2] <- getArgs
  text1 <- readFile file1
  text2 <- readFile file2
  putStrLn $ show $ fscores (readScores text1) (readScores text2)