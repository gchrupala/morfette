{-# OPTIONS -fno-monomorphism-restriction #-}
module GramLab.Statistics ( SetPair(..)
                          , Scores(..)
                          , Report(..)
                          , precision
                          , recall
                          , f_score 
                          , f_score'
                          , toScores
                          , toReport
                          , getScores
                          )

where
import Data.Set  hiding (split)
import qualified Data.List as List
import Control.Exception 
import System.Random
import System                
import Debug.Trace

data SetPair a = SetPair { testSet :: Set a , goldSet :: Set a } deriving (Show)
data Scores a = Scores { truePosCard :: a , testSetCard :: a , goldSetCard :: a } deriving (Show,Eq,Ord)

toScores :: (Num b, Ord a,Show a) => SetPair a -> Scores b

-- toScores pair | trace (show ((testSet pair)
--                             ,(goldSet pair)
--                             ,(truePos pair))) False = undefined

toScores pair = Scores { truePosCard = genericSize $ truePos pair
                       , testSetCard = genericSize $ testSet pair
                       , goldSetCard = genericSize $ goldSet pair } 


truePos     pair    = intersection (testSet pair) (goldSet pair)

precision   scores  = fromIntegral (truePosCard scores) / fromIntegral (testSetCard scores)

recall      scores  = fromIntegral (truePosCard scores) / fromIntegral (goldSetCard scores)

f_score     scores  = f_score' p r
    where p = precision scores
          r = recall    scores
f_score' p r = (2 * p * r) / (p + r) 
genericSize = fromIntegral . size

instance Functor Scores where
    fmap f (Scores a b c) = Scores (f a) (f b) (f c)
instance (Num a) => Num (Scores a) where
    (Scores a b c) + (Scores a' b' c') = Scores (a+a') (b+b') (c+c')
    (Scores a b c) - (Scores a' b' c') = Scores (a-a') (b-b') (c-c')
    (Scores a b c) * (Scores a' b' c') = Scores (a*a') (b*b') (c*c')
    abs    = fmap abs 
    negate = fmap negate
    signum = fmap signum
    --fromInteger i | trace (show i) False = undefined
    fromInteger i = Scores (fromInteger i) (fromInteger i) (fromInteger i)

data Report = Report Double Double Double deriving (Show)
--toReport scores | trace (show scores) False = undefined
toReport scores = Report (precision scores)
                         (recall    scores)
                         (f_score   scores)

getScores result =    zipWith (\t g -> toScores (SetPair (result t) (result g)))


