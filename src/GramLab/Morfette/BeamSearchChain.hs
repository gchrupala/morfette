module GramLab.Morfette.BeamSearchChain ( train
                                        , trainFun
                                        , predict 
                                        , toModelFun
                                        , FeatureSpec (..)
                                        , Smth
                                        , Tok
                                        )
where
import GramLab.Morfette.LZipper

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Dynamic
import GramLab.FeatureSet
import qualified GramLab.Maxent.ZhangLe.Model as M
import Data.Traversable (forM)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Debug.Trace
--data Smth = String String | EditScript String deriving (Eq,Ord,Show,Read)
type Smth = String

type ProbDist a = [(a,Double)]

type Tok = [Smth]
type Model = LZipper [Smth] [Smth] [Smth] -> ProbDist Smth
beamSearch :: 
              Int   -- beam size
           -> Double  -- preprune threshold 
           -> [Model]
           -> ProbDist (LZipper Tok Tok Tok) -- prob dist over sequence of "tokens in context" (as lzippers)
           -> ProbDist [Tok] -- prob dist over sequences of "tokens"
--beamSearch n th cfs pzs | trace (show (n,th,pzs)) False = undefined
beamSearch n th cfs pzs =
    if any (atEnd . fst) pzs  -- of any lzipper at end then get then return tokens
    then flip map pzs $ \(z,p) -> (map id (reverse (left z)),p)
    else -- otherwise apply each classifier in turn in the lzipper seq, prune, adjust probs
         let f model pzs' =     prune n 
                              . flip concatMap pzs'
                              $ \(z,p) -> flip map (preprune th .  model $ z) 
                                          $ \(c,c_p) -> (modify (\x -> x ++ [c]) z,p*c_p)
                           
         in beamSearch n th cfs . map (\(z,p) -> (slide z,p)) $! (foldr f pzs cfs)

-- pruning and prepruning

prune :: Int -> ProbDist a -> ProbDist a
prune n = take n . sortBy (flip (comparing snd))
collectUntil cond f z []     = []
collectUntil cond f z (x:xs) = let z' = (f $! x) $! z 
                               in  if   cond x z' then []
                                   else x: collectUntil cond f z' xs
preprune th = collectUntil (\x z -> th > snd x / z) ((+) . snd) 0

type Label = Smth

data FeatureSpec = FS { label    :: Tok -> Smth
                      , features :: LZipper Tok Tok Tok -> [Feature String Double] }

trainFun :: [[Tok]] -> [FeatureSpec] -> IO [Model]
trainFun sents fspecs = do
  ms <- train sents fspecs
  return (zipWith toModelFun fspecs ms) 
 
train :: [[Tok]] -> [FeatureSpec] -> IO [M.Model Label Int String Double]
train sents fspecs = do
  models <- forM (reverse fspecs)
                $ \fs -> do { model <- M.train params (concatMap (sentToExamples fs) sents)
                            ; return model }
  return (reverse models)
 


toModelFun :: FeatureSpec -> (M.Model Label Int String Double) -> Model
toModelFun fs m = 
    \ z -> M.distribution m (features fs z)

predict :: [[Tok]] -> [Model] -> [[Tok]]
predict sents models = map predictOne sents
    where predictOne s = fst . head . beamSearch beamSize th models $ [(fromList s,1)]

params = M.trainSettings
th = 0.3
beamSize = 3

sentToExamples ::  FeatureSpec -> [Tok] -> [(Label,[Feature String Double])]
sentToExamples fs xs = slideThru f (fromList xs)
    where f z = (label fs (fromMaybe (error "sentToExample:fromMaybe") (focus z)), features fs z)
    
slideThru f z  | atEnd z   = []
slideThru f z              = f z:slideThru f (slide z) 
