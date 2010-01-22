module GramLab.Morfette.BeamSearch ( beamSearch
                                   , beamSearchSingle
                                   , ProbDist
                                   , examples
                                   , mkTrainEx
                                   , mkTrainExSingle
                                   )
where 
import GramLab.Morfette.Features.Common
import GramLab.Morfette.LZipper
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
-- Beam search implementation


type ProbDist a = [(a,Double)]

prune :: Int -> ProbDist a -> ProbDist a
prune n = take n . sortBy (flip (comparing snd))
                                           
collectUntil cond f z []     = []
collectUntil cond f z (x:xs) = let z' = (f $! x) $! z 
                               in  if   cond x z' then []
                                   else x: collectUntil cond f z' xs


beamSearch :: (Ord pos, Ord lem) => 
              (lem -> String -> Bool)
           -> Int 
           -> Double 
           -> (LZipper (String, OR pos lem) String String -> ProbDist pos)
           -> (LZipper (String, OR pos lem) (String, OR pos lem) String -> ProbDist lem)
           -> ProbDist (LZipper (String, OR pos lem) String String)
           -> ProbDist [OR pos lem]
beamSearch check n c postagger lemmatizer pzs =
    if any (atEnd . fst) pzs 
    then flip map pzs $ \(z,p) -> (map snd (reverse (left z)),p)
    else -- for each sequence find a tag prob dist
        let pzs'  = prune n $ flip concatMap pzs  
                            $ step postagger (const id)  (\c -> modify (\form -> (form,LHS c)))
            pzs'' = prune n $ flip concatMap pzs' 
                            $ step lemmatizer lemmafilter (\c -> modify (\(form,lab) -> (form,putRight c lab)))
        in  beamSearch check n  c postagger lemmatizer . map (\(z,p) -> (slide z,p)) $! pzs''
    where step model filt mod (z,p) = flip map (preprune . filt z . model $ z)
                                 $ \ (c,c_p) ->
                                      (mod c z,p * c_p)
          preprune = collectUntil (\x z -> c > snd x / z) ((+) . snd) 0
          lemmafilter z  = filter (\(c,p) -> check c (fst . fromJust . focus $ z))

beamSearchSingle :: (Ord pos, Ord lem) => 
              (OR pos lem -> String -> Bool)
           -> Int 
           -> Double 
           -> (label -> OR pos lem)
           -> (LZipper (String, OR pos lem) String String -> ProbDist label)
           -> ProbDist (LZipper (String, OR pos lem) String String)
           -> ProbDist [OR pos lem]
beamSearchSingle check n c inject classifier pzs =
    if any (atEnd . fst) pzs 
    then flip map pzs $ \(z,p) -> (map snd (reverse (left z)),p)
    else -- for each sequence find a tag prob dist
        let pzs'  = prune n $ flip concatMap pzs  
                            $ step classifier (const id)  (\c -> modify (\form -> (form,inject c)))
        in  beamSearchSingle check n c inject classifier . map (\(z,p) -> (slide z,p)) $! pzs'
    where step model filt mod (z,p) = flip map (preprune . filt z . model $ z)
                                 $ \ (c,c_p) ->
                                      (mod c z,p * c_p)
          preprune = collectUntil (\x z -> c > snd x / z) ((+) . snd) 0
          lemmafilter z  = filter (\(c,p) -> check c (fst . fromJust . focus $ z))



                        

mkTrainEx pos lem posfeats lemfeats z tok = 
    let lab = putRight (getRight (lem tok)) (pos tok)
        z'  = modify (\form -> (form,lab)) z
    in  (z',((getLeft lab,posfeats z),(getRight lab,lemfeats z')))          

mkTrainExSingle mkLab getLab feats z tok = 
    let lab = mkLab tok
        z'  = modify (\form -> (form,mkLab tok)) z
    in (z',(getLab lab,feats z))

examples f sentence = slideThru f z sentence
    where z = fromList (map tokenForm sentence)

slideThru f z [] | atEnd z   = []
slideThru f z (x:xs)         = res:slideThru f (slide z') xs
    where (z',res) = f z x
