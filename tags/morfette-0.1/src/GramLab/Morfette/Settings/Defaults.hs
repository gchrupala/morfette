module GramLab.Morfette.Settings.Defaults ( posTrainSettings
                                          , lemmaTrainSettings
                                          )
where 
import GramLab.Maxent.ZhangLe.Model

lemmaTrainSettings = TrainSettings { iter      = 50 
                                   , gaussian  = 1 
                                   , tolerance = 1e-05 
                                   , verbose   = True 
                                   , scale     = False } 
posTrainSettings = TrainSettings { iter      = 60 
                                 , gaussian  = 1 
                                 , tolerance = 1e-05 
                                 , verbose   = True 
                                 , scale     = False } 
                   
