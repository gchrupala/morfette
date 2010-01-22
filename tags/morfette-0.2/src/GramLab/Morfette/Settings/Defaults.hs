module GramLab.Morfette.Settings.Defaults ( posTrainSettings
                                          , lemmaTrainSettings
                                          )
where 
import GramLab.Perceptron.Model

lemmaTrainSettings = TrainSettings { iter      = 10
                                   , rate      = 0.1
                                   , occurTh = 40
                                 , entropyTh = 0.0 }
posTrainSettings = TrainSettings { iter      = 20
                                 , rate      = 0.1
                                 , occurTh = 40
                                 , entropyTh = 0.0 }
                   
