module GramLab.Utils ( join
                     , splitOn
                     , splitWith
                     , splitInto
                     , padLeft
                     , padRight
                     , makeList 
                     , lowercase
                     , uppercase
                     , at
                     , distribute
                     , distributeOver
                     , cumulDifference
                     , index
                     , unfoldrM
                     , tokenize
                     )
    
where
import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Control.Monad.State as S
import Control.Monad (liftM)
import Debug.Trace
-- Lists/Strings
join        :: [a] -> [[a]] -> [a]
join sep xs = concat $ intersperse sep xs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn = splitWith . (==)

splitWith ::  (a -> Bool) -> [a] -> [[a]]
splitWith f s =  case dropWhile f s of
                   [] -> []
                   s' -> w : splitWith f s''
                       where (w, s'') = break f s'

splitInto :: Int -> [a] -> [[a]]
splitInto size [] = []
splitInto size xs = let (ws,ys) = splitAt size xs
                    in  ws:splitInto size ys

padLeft              :: a -> Int -> [a] -> [a]
padLeft  pad size xs = reverse $ padRight pad size $ reverse xs

padRight             :: a -> Int -> [a] -> [a]
padRight pad size xs = take size (xs ++ (repeat pad))

lowercase :: String -> String
--lowercase xs | trace xs False = undefined
lowercase xs = map toLower xs

uppercase :: String -> String
uppercase  = map toUpper 

makeList size elt = take size (repeat elt)

at err  m k = case Map.lookup k m of
           Nothing -> error (err k)
           Just x  -> x

distributeOver   :: Int -> [t] -> [[t]]
distributeOver i = distribute ([],(replicate i []))

distribute (xs,ys)   []     = xs ++ ys
distribute (xs,[])   zs     = distribute ([],xs) zs
distribute (xs,y:ys) (z:zs) = distribute ((z:y):xs,ys) zs

cumulDifference :: (Ord a) => [Set a] -> [Set a]
cumulDifference = snd . mapAccumL (\z x -> (z `Set.union` x, x `Set.difference` z)) Set.empty 

index xs = S.evalState (T.mapM count xs) 0
    where count a = do
            i <- S.get
            S.put (i+1)
            return (i,a)
unfoldrM f b  = do 
  result <- f b
  case result of
    Just (a,new_b) -> liftM (a:) (unfoldrM f new_b)
    Nothing        -> return []

sepPunct :: String -> [String]
sepPunct xs = let (before,rest) = span isPunctuation xs
                  (after,this)  = span isPunctuation (reverse rest)
              in filter (not . null) [before,reverse this,reverse after]
tokenize :: String -> [String]
tokenize = concatMap sepPunct . words