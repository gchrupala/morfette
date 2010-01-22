{-# LANGUAGE NoMonomorphismRestriction#-} 
module GramLab.Data.CommonSubstrings ( lcs
                                     , lcsFiltering
                                     , commonSubstrings
                                     , substrings
                                     , toMap
                                     , CommonSubstring
                                     , Indices
                                     )

where

import Data.List
import qualified Data.Map as Map
import Data.Ord
import Data.Char
import qualified GramLab.Data.StringLike as S


type Indices = ([Int],[Int])
commonSubstrings :: (Ord s, Ord a, S.StringLike s a) => s -> s -> Map.Map s Indices
commonSubstrings xs ys = Map.intersectionWith (,) ((toMap . substrings) xs)
                                                  ((toMap . substrings) ys)

commonSubstringsWith equiv xs ys = [ (x,y,i_x,i_y) | (x,i_x) <- (Map.toList . toMap . substrings) xs
                                                   , (y,i_y) <- (Map.toList . toMap . substrings) ys
                                                   , x `equiv` y ]
collapseVowels []  = []
collapseVowels [x] = [x]
collapseVowels (x:y:xs) | vowel x && vowel y = '@':collapseVowels xs
                        | vowel x            = '@':collapseVowels (y:xs)
                        | otherwise          =  x :collapseVowels (y:xs)
   where vowel x = x `elem` "ieaou"

equiv xs ys = collapseVowels xs == collapseVowels ys

type  CommonSubstring s = (s,Indices)

longest :: (Ord s, Ord a, S.StringLike s a) => Map.Map s Indices -> [CommonSubstring s]
longest = sortBy (flip (comparing (S.length . fst))) . Map.toList

lcs :: (Ord a, Ord s, S.StringLike s a, Monad m) =>
       s -> s -> m (CommonSubstring s)
lcs = lcsFiltering (const True)
lcsFiltering :: (Ord s, Ord a, S.StringLike s a, Monad m) => 
                (CommonSubstring s -> Bool) 
             -> s 
             -> s 
             -> m (CommonSubstring s)
lcsFiltering f xs ys = case longest (Map.filterWithKey (curry f) (commonSubstrings xs ys)) of
                         [] -> fail "lcsFiltering: no common strings"
                         (x:_) -> return x

substrings :: (Enum b, Num b,Ord a, S.StringLike s a) => s -> [(s, b)]
substrings xs = [ (S.fromString $ map fst suffix, snd . head $ suffix) 
                      | prefix <- tail $ inits' (zip (S.toString xs) [0..])
                      , suffix <- init $ tails prefix ]
inits' = reverse . map reverse . tails . reverse

toMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
toMap = foldr (\(k,v) z -> Map.insertWith (++) k [v] z) Map.empty
