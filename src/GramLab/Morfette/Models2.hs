module GramLab.Morfette.Models2 ( train
                               , trainFun
                               , predict 
                               , predictPipeline
                               , toModelFun
                               , mkPreprune
                               , sentToExamples
                               , FeatureSpec (..)
                               , Row(..)
                               )
where
import GramLab.Morfette.LZipper
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List (sortBy,foldl',transpose)
import Data.Ord (comparing)
import Data.Dynamic
import GramLab.FeatureSet
import qualified GramLab.Perceptron.Model as M
import Data.Traversable (forM)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.Binary
import Control.Monad (liftM, liftM2)
import GramLab.Utils (uniq)
import qualified Data.Vector.Unboxed as U
import GramLab.Morfette.BinaryInstances

data Row t a = Row { input :: !t, output :: ![a] } 
               deriving (Eq,Ord,Show,Read)


data FeatureSpec t a = 
    FS { label    :: Row t a -> a
       , features :: LZipper (Row t a) (Row t a) (Row t a) -> [Feature String Double] 
       , preprune :: ProbDist a -> ProbDist a
       , check    :: LZipper (Row t a) (Row t a) (Row t a) -> a -> Bool
       , pruneUniqLabels :: Bool
       , trainSettings :: M.TrainSettings }
    
    
instance (Binary t, Binary a) => Binary (Row t a) where
  put t = put (input t) >> put (output t)
  get = liftM2 Row get get
    

type ProbDist a = [(a,Double)]

type Model t a = LZipper (Row t a)  (Row t a) (Row t a) -> ProbDist a

beamSearch :: 
              Int   
              -- beam size
           -> [Model t a] 
              -- models for each output column
           -> ProbDist (LZipper (Row t a) (Row t a) (Row t a)) 
              -- prob dist over sequence of "tokens in context" (as lzippers)
           -> ProbDist [Row t a] 
              -- prob dist over sequences of "tokens"
beamSearch n ms pzs =
  let apply pzs model = 
        prune n 
          [ (modify (\t -> t { output = output t ++ [label] }) z  -- add label to output
          , p0 * p                                                -- multiply probability in
          ) 
          | (z, p0) <- pzs                                        -- for each sequence (lzipper)
          , (label, p) <- model z                                 -- get label and prob by applying
                                                                  -- model. 
          ]
  in if any (atEnd . fst) pzs  
        -- of any lzipper at end then return tokens
     then flip map pzs $ \(z,p) -> (reverse (left z), p)
     else 
       -- otherwise apply both classifiers in turn in the lzipper seq,
       -- prune, adjust probs
       beamSearch n ms . map (\(z,p) -> (slide z,p)) $! (foldl' apply pzs ms)

-- pruning and prepruning

prune :: Int -> ProbDist a -> ProbDist a
prune n = take n . sortBy (flip (comparing snd))
collectUntil cond f z []     = []
collectUntil cond f z (x:xs) = let z' = (f $! x) $! z 
                               in  if   cond x z' then []
                                   else x: collectUntil cond f z' xs
mkPreprune th = collectUntil (\x z -> th > snd x / z) ((+) . snd) 0

trainFun ::  (Ord a, Show a) => [FeatureSpec t a] -> [[Row t a]]  -> [Model t a]
trainFun fspecs sents = 
  let ms = train fspecs sents
  in (zipWith toModelFun fspecs ms) 
 
train :: (Ord a,Show a) => 
         [FeatureSpec t a] 
      -> [[Row t a]]  
      -> [M.Model a Int String Double]
train fspecs sents = 
  flip map fspecs
           $ \fs ->  let yxs_all = concatMap (sentToExamples fs) $ sents
                         zs_all  = concat [ take (length s) 
                                        . iterate slide 
                                        . fromList
                                        $ s 
                                        | s <- sents ]
                         ys  = (if pruneUniqLabels fs then Map.filter (>1) else id)
                               . Map.fromListWith (+)
                               . map (\ (y, _) -> (y, 1))
                               $ yxs_all
                         (yxs, zs) = unzip
                                     . filter (flip Map.member ys . fst . fst)
                                     $ zip yxs_all zs_all  
                         yss = [ [ y | y <- Map.keys ys , check fs z y ] 
                                 | z <- zs ]
                     in M.train (trainSettings fs) yss yxs

toModelFun :: (Ord a, Show a) => 
              FeatureSpec t a 
           -> (M.Model a Int String Double) 
           -> Model t a
toModelFun fs m = 
    let ys = Map.keys . M.classMap . M.modelData $ m
    in
    \ z -> case filter (check fs z) ys of
             [] -> error "GramLab.Morfette.Models.toModelFun: unexpected []"
             y:ys' -> 
                 preprune fs 
                  . M.distribution m (y:ys')
                  . features fs 
                  $ z 
           
predict :: Int -> Int -> [Model t a] -> [[Row t a]] -> [[[Row t a]]]
predict k beamSize models sents = map predictK sents
    where predictK s = transpose 
                         . map fst 
                         . take k
                         . beamSearch beamSize models 
                         $ [(fromList s,1)]

predictPipeline :: Int -> [Model t a] -> [[Row t a]] -> [[Row t a]]
predictPipeline beamSize models sents = map predictK sents
    where predictK s = foldl' (\s1 m -> fst . head . beamSearch beamSize [m] 
                                         $ [(fromList s1,1)]) s models
                               

sentToExamples ::  FeatureSpec t a 
               -> [Row t a] 
               -> [(a,[Feature String Double])]
sentToExamples fs xs = slideThru f (fromList xs)
    where f z = 
              (  label fs 
               . fromMaybe
                     (error "GramLab.Morfette.Models.sentToExample:fromMaybe")
               . focus 
               $  z
              , features fs z)
    
slideThru f z  | atEnd z   = []
slideThru f z              = f z:slideThru f (slide z) 
