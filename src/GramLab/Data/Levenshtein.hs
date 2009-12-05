import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace
--levenshtein::String->String->Int
levenshtein s t = 
    d -- !!(length s)!!(length t) 
    where d = [ [ distance m n | n <- [0..length t] ] | m <- [0..length s] ]
          distance i 0 = i
          distance 0 j = j
          distance i j = minimum [ d!!(i-1)!!j+1
                                 , d!!i!!(j-1)+1
                                 , d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 2) 
                                 ]

levenshtein' s t = 
    d -- !!(length s)!!(length t) 
    where d = Map.fromList [ ((a,b,m,n),distance a b m n) | a <- [0..length s] , b <- [0..length t] 
                                                          , m <- [0..length s] , n <- [0..length t] 
                                                          , a<=m && b<=n ]

--          distance a b i j | trace ("=>" ++ show (a,b,i,j)) False = undefined
          distance a b i j | a == i = j - b
          distance a b i j | b == j = i - a
          distance 0 0 i 0 = i
          distance 0 0 0 j = j
          distance 0 0 i j = minimum [ d`at`(0,0,i,j-1)+1
                                     , d`at`(0,0,i-1,j)+1
                                     , d`at`(0,0,i-1,j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 2)
                                     ] 
          distance 0 b i j | i == lens && j == lent = b
          distance a 0 i j | i == lens && j == lent = a
          distance 0 b i j = minimum [ d`at`(0,b,i,j-1)+1
                                     , d`at`(0,b,i-1,j)+1
                                     , d`at`(0,b-1,i,j)+1
                                     , d`at`(0,b,i-1,j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 2)
                                     ]
          distance a 0 i j = minimum [ d`at`(a,0,i,j-1)+1
                                     , d`at`(a,0,i-1,j)+1
                                     , d`at`(a-1,0,i,j)+1
                                     , d`at`(a,0,i-1,j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 2)
                                     ]
          distance a b i j = minimum [ d`at`(a,b,i,j-1)+1
                                     , d`at`(a,b,i-1,j)+1
                                     , d`at`(a,b-1,i,j)+1
                                     , d`at`(a-1,b,i,j)+1
                                     , d`at`(a,b,i-1,j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 2)
                                     , d`at`(a-1,b-1,i,j) + (if s!!(a-1)==t!!(b-1) then 0 else 2)
                                     ]
          lens = length s
          lent = length t
m `at` elt = case Map.lookup elt m of
               Nothing -> error $ "Element " ++ show elt ++ " not in the map"
               Just x  -> x

