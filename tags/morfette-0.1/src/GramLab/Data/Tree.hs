{-# OPTIONS -fno-monomorphism-restriction #-}
module GramLab.Data.Tree ( Tree(..) 
                    , map
                    , mapTopDown
                    , mapAccumPre
                    , mapAccumPost
                    , mapAccumLevel
                    , o
                    , o'
                    , foldl
                    , foldr
                    , nodeSpan
                    , nodeSpanSkipSome
                    , leaves
                    , nodeSpecs
                    , nodes
                    )
where 
import Prelude hiding (map,foldl,foldr)
import Data.Tree
import qualified Data.List as List
import qualified Data.Set as Set

map f (Node l ns) = f (l, (List.map (map f) ns)) -- Note that: map id   t /= t
                                                 -- However:   map (uncurry Node) t == t
-- Top down traversal
-- Maps tree to tree of the same type
mapTopDown f (Node l ns) = (Node l' ns'')
    where (Node l' ns')  = f (l,ns)
          ns''           = List.map (mapTopDown f) ns'

-- The descriptions of traversal order are a bit imprecise.
-- They reflect accurately how the seed is passed around. But nodes are always visited
-- bottom up (?)
-- FIXME Add some QuickCheck tests for those.
-- Preorder (DLR) traversal
mapAccumPre f z (Node l ns) = (z'',x)
    where (z',x)    = f z (l, ns') 
          (z'',ns') = List.mapAccumL (mapAccumPre f) z' ns
-- Postorder (LRD) traversal
mapAccumPost f z (Node l ns) = f z' (l, ns')
    where (z',ns') = List.mapAccumL (mapAccumPost f) z ns

mapAccumLevel  f z (Node l ns) = (z'',x')
    where (z',x) = f z (l,ns)
          (z'',x') = mapAccumLevel' f z' x

mapAccumLevel' f z (Node l ns) = (z'',(Node l ns''))
    where (z',ns') = List.mapAccumL (\z (Node x ys) -> f z (x,ys)) z ns
          (z'',ns'') = List.mapAccumL (mapAccumLevel' f) z' ns'

-- Function composition specialized to work with the functional argument to map
f `o` g  = \(l,ns) -> let (Node l' ns') = f (l,ns) in g (l',ns')
-- Function composition specialized to work with the functional argument to mapAccum*
f `o'` g = \acc (x,ys) -> let (acc',(Node l ns)) = g acc (x,ys)
                               in  f acc' (l,ns)

-- Node terminal span. To annotate a Rose Tree with spans:
-- mapAccumL (nodeSpan (,) snd) 0


-- For skippable nodes (where f x is True), we add span which is equal to the span of the next word
nodeSpan  = nodeSpanSkipSome (const False) 
nodeSpanSkipSome f put get i (x, []) 
    = (if f x then i else i+1,(Node (put x (i,i)) []))
nodeSpanSkipSome f put get i (x, ys)  = (i,  (Node (put x (j,k)) ys))
    where (j,k) = case ys of
                    [y]     -> get . rootLabel $ y
                    (y:ys)  -> (fst . get . rootLabel $ y 
                               -- Here the last child could be skippable; if so make k one less
                               , let kl =  rootLabel . last $ ys in (snd . get $ kl) - (if f kl then 1 else 0))

-- Folds specialized to Rose Trees
foldr f z   (Node l ns) = f (l, ns) z'
    where z' = List.foldr (\n z -> foldr f z n) z ns
foldl f z   (Node l ns) = f z' (l, ns)
    where z' = List.foldl (\z n -> foldl f z n) z ns

nodes = foldr (\(l, ns) z -> l:z) []

leaves :: Tree a -> [a]
leaves =  foldr (\(x, ns) z -> if null ns then x:z else z) []

nodeSpecs :: (Ord a) => (t -> a) -> Tree t -> Set.Set a
nodeSpecs  get = foldr (\(x, ns) z -> Set.insert (get x) z) Set.empty
