module GramLab.Morfette.Config ( Config(..) 
                               , defaults 
                               )
where 
import GramLab.Maxent.ZhangLe.Model
    

data Config = Config { lemmaConfig   :: TrainSettings
                     , posConfig :: TrainSettings } deriving (Show,Read) 
            
defaults = Config { lemmaConfig = TrainSettings { iter      = 50
                                                , rate      = 0.1 
                                                , occurTh   = 40
                                                , entropyTh = 0.0 } 
                  , posConfig = TrainSettings { iter      = 60 
                                              , rate    = 0.1
                                              , occurTh = 40
                                              , entropyTh = 0.0 } }
