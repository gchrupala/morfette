module GramLab.Morfette.Config ( Config(..) 
                               , defaults 
                               )
where 
import GramLab.Maxent.ZhangLe.Model
    

data Config = Config { lemmaConfig   :: TrainSettings
                     , posConfig :: TrainSettings } deriving (Show,Read) 
            
defaults = Config { lemmaConfig = TrainSettings { iter      = 50 
                                                , gaussian  = 1 
                                                , tolerance = 1e-05 
                                                , verbose   = True 
                                                , scale     = False } 
                  , posConfig = TrainSettings { iter      = 60 
                                                  , gaussian  = 1 
                                                  , tolerance = 1e-05 
                                                  , verbose   = True 
                                                  , scale     = False } }
