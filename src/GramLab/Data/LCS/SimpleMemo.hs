-----------------------------------------------------------------------------
-- Memoized version of Simple.hs [1] but still simple stupid and slow. 
-- foldlcs lcs and ses adapted from Gauche's util.lcs module [2]
-- [1] http://urchin.earth.li/darcs/ian/lcs/Data/List/LCS/Simple.hs
-- [2] http://www.shiro.dreamhost.com/scheme/gauche/memo.html
module GramLab.Data.LCS.SimpleMemo ( Edit (Ins,Del)
                                   , EditScript
                                   , lcs
                                   , ses 
                                   , apply
                                   , check
                                   ) 
where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Typeable
import Prelude hiding (lookup)
import Maybe (catMaybes)
import Debug.Trace
data Edit a       = Ins a Int
                  | Del a Int deriving (Eq,Ord,Show,Read)
type EditScript a = [Edit a]

-- |The 'lcs' function takes two lists and returns a list with a longest
-- common subsequence of the two.
lcs :: (Ord a, Eq a) => [a] -> [a] -> [a]
lcs xs ys = map (\(x,_,_) -> x)  (snd (lcsWithPositions xs ys))

-- |The 'ses' function takes two lists and returns the shortest edit script
-- which would change the first list into the second
ses :: (Ord a, Eq a) => [a] -> [a] -> EditScript a
ses as bs =
    reverse changes
        where aOnly a ((aPos,bPos),zs) = ((aPos+1,bPos),(Del a aPos) : zs)
              bOnly b ((aPos,bPos),zs) = ((aPos,bPos+1),(Ins b aPos) : zs)
              both  c ((aPos,bPos),zs) = ((aPos+1,bPos+1),zs)
              (_,changes) = foldlcs aOnly bOnly both ((0,0),[]) as bs

-- |The 'apply' function takes an edit script and a sequence and applies the
-- changes encoded in the script to the sequence.
-- The following holds: apply (ses xs ys) xs == ys
apply :: EditScript a -> [a] -> [a]
apply s xs = concat $ applyIns s $ applyDel s xs

foldlcs aOnly bOnly both seed a b =
    loop common seed a 0 b 0
    where fold f z xs = foldl (\z x -> f x z) z xs
          (len,common) = lcsWithPositions a b
          loop common seed a aPos b bPos 
              | null common  = fold bOnly (fold aOnly seed a) b
              | otherwise    = 
                  let ((e,aOff,bOff):commonTail) = common
                      aSkip = aOff - aPos
                      bSkip = bOff - bPos
                      (aHead,aTail) = splitAt aSkip a
                      (bHead,bTail) = splitAt bSkip b
                  in loop commonTail
                          (both e (fold bOnly (fold aOnly seed aHead) bHead))
                          (tail aTail)
                          (aOff + 1)
                          (tail bTail)
                          (bOff + 1)

type PosSpec a = (a,Int,Int)
type LCSMemoTable a =  Map.Map ((Int,[a]),(Int,[a])) (Int,[PosSpec a])               

lcsWithPositions :: (Ord a) => [a] -> [a] -> (Int,[PosSpec a])
lcsWithPositions xs ys = evalState (lcs_memo (0,xs) (0,ys)) Map.empty 

lcs_memo :: (Ord a) => (Int,[a]) -> (Int,[a]) -> State (LCSMemoTable a) (Int,[PosSpec a])
lcs_memo xs ys = do
    d <- get
    case Map.lookup (xs,ys) d of
        Nothing -> do
            r <- lcs' lcs_memo xs ys
            modify (Map.insert (xs,ys) r)
            return r
        Just r -> return r
 
lcs' rec (px,x:xs) (py,y:ys)
 | x == y = do 
    (len,zs) <- rec (px+1,xs) (py+1,ys)
    return (len + 1, (x,px,py):zs)
 | otherwise = do 
    r1@(l1, _) <- rec (px,  x:xs) (py+1,ys)
    r2@(l2, _) <- rec (px+1,xs)   (py,  y:ys)
    if l1 >= l2 then return r1 else return r2
lcs' _ (_,[]) _     = return (0, [])
lcs' _  _    (_,[]) = return (0, [])


pos (Del _ i) = i
pos (Ins _ i) = i
isIns (Ins _ _) = True
isIns (Del _ _) = False
isInsAt j (Ins _ i) = i == j
isInsAt _ _         = False
applyDel s xs = 
    applyDel' 0 s' xs
    where s' = filter (not . isIns) s
applyIns s xs =
    applyIns' 0 s' xs
    where s' = filter isIns s

applyDel' i (e@(Del _ j):es) (x:xs)
    | i == j    =  [] : applyDel' (i+1) es xs
    | otherwise =  [x]  : applyDel' (i+1) (e:es) xs
applyDel' i [] xs = map (:[]) xs
applyDel' i es [] = []

applyIns' i es (x:xs) = (insertions ++ x) : applyIns' (i+1) es xs
    where insertions = insertionsAt i es
applyIns' i es [] = [insertionsAt i es]

insertionsAt i es = concatMap (\(Ins c j) -> if j == i then [c] else []) es

check es xs = check' 0 es xs
--check' i es xs | trace (show (i,es,xs)) False = undefined
check' i (e@(Del c j):es) (x:xs) 
      | i == j     = c == x && check' (i+1) es     xs 
      | otherwise  = check' (i+1) (e:es) xs
check' i (e@(Ins c j):es) (x:xs)
      | i == j     = check' (i+1) (dropWhile (isInsAt i) es) xs
      | otherwise  = check' (i+1) (e:es) xs
check' i [] xs = True
check' i (Del {}:_) []  = False
check' i (Ins {}:es) [] = check' (i+1) es [] 



testset = [  ("carbon","cabron"),("otreum","rirom"),("evita","pabita")
           , ("abcabba","cbabac"),("caballo","lacayo"),("dijeran","decir")
           , ("joroba","jodieron"),("alzheimer","heimat"),("argentina","argentaria")
           , ("morir","murieron"),("ordenadores","computadoras"),("toalla","oatmeal")
          ]
runtest = mapM_ print $ map (\(a,b) -> ((a,b),"=>",(apply (ses a b) a) == b)) testset

