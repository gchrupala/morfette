module GramLab.Morfette.LZipper ( LZipper
                                , focus
                                , left
                                , right
                                , reset
                                , fromList
                                , modify
                                , slide
                                , slideWith
                                , slideWith1
                                , atEnd )
where
import Data.Maybe
import Debug.Trace

data LZipper a b c = LZ [a] (Maybe b) [c]  deriving (Show,Eq,Ord)
left  (LZ xs _ _) = xs
focus (LZ _ x _)  = x
right (LZ _ _ ys) = ys

fromList []     = LZ [] Nothing [] 
fromList (x:xs) = LZ [] (Just x) xs

modify :: (b -> c) -> LZipper a b d -> LZipper a c d
--modify f z@(LZ xs x ys)   | trace (show (z,(LZ xs (fmap f x) ys)))  False = undefined 
modify f (LZ xs x ys) = LZ xs (fmap f x) ys

slide :: LZipper t t c -> LZipper t c c
slide = slideWith id id

slideWith1 :: (c -> b) -> LZipper t t c -> LZipper t b c
slideWith1 = slideWith id

slideWith :: (t -> a) -> (c -> b) -> LZipper a t c -> LZipper a b c
slideWith f g (LZ xs Nothing []) = LZ xs Nothing []
slideWith f g (LZ lxs (Just x) rxs)  =
    case rxs of
      y:ys -> LZ (f x:lxs) (Just (g y)) ys
      []   -> LZ (f x:lxs) Nothing []

atEnd = isNothing . focus

reset (LZ [] (Just y) ys)     = LZ [] (Just y) ys
reset (LZ [] Nothing [])      = LZ [] Nothing []
reset (LZ xs (Just y) ys) = LZ [] (Just z) (zs++[y]++ys)
    where z:zs = reverse xs                    
reset (LZ xs Nothing [])  = LZ [] (Just z) zs
    where z:zs = reverse xs
