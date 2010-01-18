module GramLab.Morfette.Evaluation ( accuracy
                                   , tokenAccuracy
                                   , sentenceAccuracy
                                   , showAccuracy
                                   )
where
import GramLab.Morfette.Token
import Data.Maybe
import GramLab.Utils (lowercase)
import Text.Printf
import GramLab.Binomial


data Accuracy = Acc { lemmaAcc :: AccBL
                    , posAcc   :: AccBL 
                    , jointAcc :: AccBL }

data AccBL = AccBL { test :: [Bool]
                   , baseline :: Maybe [Bool] }

data Experiment = Ex { trials :: Integer
                     , successes :: Integer } deriving (Show)


rer hi lo = ((1-lo)-(1-hi))/(1-lo)

significance :: [Bool] -> [Bool] -> Experiment
significance test baseline = Ex { trials = fromIntegral $ countTrue $ zipWith (/=) test baseline
                                , successes = fromIntegral $ countTrue $ zipWith (>) test baseline  }
countTrue = sum . map fromEnum
pvalue ex = binomialTest (trials ex) (successes ex)



tokenAccuracy :: [Token] -> [Token] -> Maybe [Token] -> Accuracy
tokenAccuracy xs ys bl = accuracy (id,id,id) (map tokToPair xs) 
                                             (map tokToPair ys) 
                                             (fmap (map tokToPair) bl)

sentenceAccuracy :: [[Token]] -> [[Token]] -> Maybe [[Token]] -> Accuracy
sentenceAccuracy xs ys bl = accuracy (map,map,map) (map (map tokToPair) xs) 
                                                   (map (map tokToPair) ys) 
                                                   (fmap (map (map tokToPair)) bl)
tokToPair (form,lemma,pos) = (lowercase $ fromMaybe form lemma,pos)


accuracy (g,h,i) gold test mbl = Acc { lemmaAcc = AccBL (correct (g fst) test) (fmap (correct (g fst)) mbl)
                                     , posAcc   = AccBL (correct (h snd) test) (fmap (correct (h snd)) mbl)
                                     , jointAcc = AccBL (correct (i id)  test) (fmap (correct (i id)) mbl) }
    where correct f ys = zipWith (\x y -> f x == f y) gold ys





showAccuracy acc = unlines [ "Lemma acc: " ++ (showAcc . lemmaAcc) acc
                           , "POS   acc: " ++ (showAcc . posAcc  ) acc 
                           , "Joint acc: " ++ (showAcc . jointAcc) acc ]


showAcc (AccBL t mbl) = case mbl of
                            Just bl -> let acc_bl  = (length bl, countTrue bl)
                                           acc_bl_pc = (fromIntegral (snd acc_bl)/(fromIntegral (fst acc_bl)))::Double
                                           ex = significance t bl
                                       in printf "%05.2f%% (%d / %d) %+.2f%%, %.2f%% RER, %d different, %d better, p-value: %g"
                                          (acc_t_pc * 100)
                                          (snd acc_t) 
                                          (fst acc_t) 
                                          (acc_t_pc * 100  - acc_bl_pc * 100)
                                          (rer acc_t_pc acc_bl_pc * 100)
                                          (trials ex)
                                          (successes ex)
                                          (pvalue ex)            
                            Nothing -> printf "%.2f%% (%d / %d)" (100*acc_t_pc) (snd acc_t) (fst acc_t) 
    where acc_t   = (length t, countTrue t)
          acc_t_pc = (fromIntegral (snd acc_t)/(fromIntegral (fst acc_t)))


