module GramLab.Morfette.Stat ( precision
            , recall
            , f_score
            , stats 
            , Stats(..) )
                   
where
import Data.List
precision true_pos test = true_pos / test
recall    true_pos gold = true_pos /gold
f_score   true_pos gold test = 
    f p r
        where p = precision true_pos test
              r = recall    true_pos gold

f p r =     (2 * p * r) / (p + r) 

data Stats = Stats { statsBaseAccuracy :: Double 
                   , statsAccuracy     :: Double
                   , statsPrecision    :: Double
                   , statsRecall       :: Double
                   , statsFscore       :: Double }

stats isNull gold test = Stats { statsBaseAccuracy = baseAccuracy
                               , statsAccuracy     = accuracy
                               , statsPrecision    = p 
                               , statsRecall       = r 
                               , statsFscore       = f p r }
    where pairs = zip gold test
          size = genericLength gold
          baseAccuracy = genericLength (filter isNull gold) / size
          accuracy = genericLength (filter (uncurry (==)) pairs) / size 
          (gc,tc,tp) = truePositives isNull pairs
          p = precision tp tc
          r =  recall   tp gc

truePositives isNull xs = truePositives' (0,0,0) isNull xs
    
truePositives' (g_count,t_count,tp_count) isNull ((gold,test):xs)  =
    truePositives' ( if isNull gold then g_count else g_count + 1
                   , if isNull test then t_count else t_count + 1
                   , if    ((not . isNull) gold)  && gold == test
                     then tp_count + 1
                     else tp_count )
                   isNull
                   xs
truePositives' counts _ [] = counts

                  
                      
    

