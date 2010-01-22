module Main
where
import GramLab.Commands
import GramLab.Morfette.Utils

main = defaultMain commands "Usage: morfette command [OPTION...] [ARG...]"

commands = [
             ("train" , CommandSpec train
                          "train maxent model"
                          [ Option [] ["model-prefix"] (ReqArg ModelPrefix "STRING")
                                       "path prefix to model files"
                          , Option [] ["config-file"] (ReqArg ConfigFile "PATH")
                                       "path to config file"
                          ] )
           , ("predict" , CommandSpec predict
                            "predict lemmas using saved model data"
                            [ Option [] ["beam"]   (ReqArg (BeamSize . read) "+INT")
                                         "beam size to use"
                            , Option [] ["preprune","preprune-threshold"]  (ReqArg (Preprune . read) "[0..1]")
                                         "prepruning threshold" 
                            , Option [] ["eval"]   (NoArg Eval)
                                         "evaluate lemmatization accuracy"
                            ] )
{-
           , ("eval" , CommandSpec eval 
                          "evaluate morpho-tagging and lemmatization results"
                          [])
-}
           ]

