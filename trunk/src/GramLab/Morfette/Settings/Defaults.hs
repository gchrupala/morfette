module GramLab.Morfette.Settings.Defaults ( posTrainSettings
                                          , lemmaTrainSettings
                                          )
where 
import GramLab.Maxent.ZhangLe.Model

lemmaTrainSettings = TrainSettings { iter      = 50 
                                   , occurTh = 40
                                 , entropyTh = 0.0 }
posTrainSettings = TrainSettings { iter      = 60 
                                 , occurTh = 40
                                 , entropyTh = 0.0 }
                   
