module GramLab.Morfette.Models ( train
                               , trainFun
                               , predict 
                               , predictPipeline
                               , toModelFun
                               , mkPreprune
                               , FeatureSpec (..)
                               , Smth(..)
                               , Tok
                               )
where
import GramLab.Morfette.LZipper
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Dynamic
import GramLab.FeatureSet
import qualified GramLab.Perceptron.Model as M
import Data.Traversable (forM)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.Binary
import Control.Monad (liftM)
import GramLab.Utils (uniq)

data Smth a = Str { str :: String } | ES { es :: a } deriving (Eq,Ord,Show,Read)
instance Binary a => Binary (Smth a) where
    put (Str s) = put (0::Word8) >> put s
    put (ES  s) = put (1::Word8) >> put s
    get = do
      tag <- get
      case tag::Word8 of
           0 -> liftM Str get
           1 -> liftM ES get

type ProbDist a = [(a,Double)]

type Tok a = [Smth a]
type Model a = LZipper [Smth a] [Smth a] [Smth a] -> ProbDist (Smth a)
beamSearch :: 
              Int   -- beam size
           -> [Model a]
           -> ProbDist (LZipper (Tok a) (Tok a) (Tok a)) 
           -- prob dist over sequence of "tokens in context" (as lzippers)
           -> ProbDist [Tok a] -- prob dist over sequences of "tokens"
beamSearch n cfs pzs =
    if any (atEnd . fst) pzs  
    -- of any lzipper at end then get then return tokens
    then flip map pzs $ \(z,p) -> (map id (reverse (left z)),p)
    else -- otherwise apply each classifier in turn in the lzipper seq,
         -- prune, adjust probs
         let f pzs' model =   prune n 
                            . flip concatMap pzs'
                            $ \(z,p) -> 
                                flip map (model $ z) 
                                  $ \(c,c_p) -> 
                                      (modify (\x -> x ++ [c]) z,p*c_p)
         in beamSearch n cfs . map (\(z,p) -> (slide z,p)) $! (foldl f pzs cfs)

-- pruning and prepruning

prune :: Int -> ProbDist a -> ProbDist a
prune n = take n . sortBy (flip (comparing snd))
collectUntil cond f z []     = []
collectUntil cond f z (x:xs) = let z' = (f $! x) $! z 
                               in  if   cond x z' then []
                                   else x: collectUntil cond f z' xs
mkPreprune th = collectUntil (\x z -> th > snd x / z) ((+) . snd) 0

data FeatureSpec a = 
    FS { label    :: Tok a -> Smth a
       , features :: LZipper (Tok a) (Tok a) (Tok a) -> [Feature String Double] 
       , preprune :: ProbDist (Smth a) -> ProbDist (Smth a)
       , check    :: LZipper (Tok a) (Tok a) (Tok a) -> Smth a -> Bool 
       , trainSettings :: M.TrainSettings }

trainFun ::  (Ord a,Show a) => [FeatureSpec a] -> [[Tok a]]  -> [Model a]
trainFun fspecs sents = 
  let ms = train fspecs sents
  in (zipWith toModelFun fspecs ms) 
 
train :: (Ord a,Show a) => 
         [FeatureSpec a] 
      -> [[Tok a]]  
      -> [M.Model (Label a) Int String Double]
train fspecs sents = 
  flip map fspecs
           $ \fs ->  let yxs = concatMap (sentToExamples fs) $ sents
                         ys  = uniq . map fst $ yxs
                         zs  = concat [ take (length s) 
                                        . iterate slide 
                                        . fromList
                                        $ s 
                                        | s <- sents ]
                         yss = [ [ y | y <- ys , check fs z y ] 
                                 | z <- zs ]
                     in M.train (trainSettings fs) yss yxs

toModelFun :: (Ord a,Show a) => 
              FeatureSpec a 
           -> (M.Model (Label a) Int String Double) 
           -> Model a
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
           
predict :: Int -> [Model a] -> [[Tok a]] -> [[Tok a]]
predict beamSize models sents = map predictOne sents
    where predictOne s = fst 
                         . head 
                         . beamSearch beamSize models 
                         $ [(fromList s,1)]

predictPipeline :: Int -> [Model a] -> [[Tok a]] -> [[Tok a]]
predictPipeline beamSize models sents = map predictOne sents
    where predictOne s = foldl (\s1 m -> fst . head . beamSearch beamSize [m] 
                                         $ [(fromList s1,1)]) s models
                               

type Label a = Smth a
sentToExamples ::  FeatureSpec a 
               -> [Tok a] 
               -> [(Label a,[Feature String Double])]
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
