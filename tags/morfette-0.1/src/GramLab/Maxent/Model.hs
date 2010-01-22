module GramLab.Maxent.Model ( Model (..)
                            )
where
import GramLab.Data.Assoc
import GramLab.Maxent.FeatureSet

data TrainSettings { iter      :: Int
                   , gaussian  :: Double
                   , tolerance :: Double }

type Example label features = (label,features)

class Model model label featset | model -> label featset where
    train    :: TrainSettings -> [Example label featset] -> model
    predict  :: (Assoc model -> Example label featset -> 