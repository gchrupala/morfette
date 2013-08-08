module POS (featureSpec)
where
import GramLab.Morfette.Features.Common
import qualified GramLab.Data.Diff.EditTree as E
import qualified Data.Map as Map
import Lemma (apply)
import qualified Data.Vector.Unboxed as U

featureSpec global = FS { label    = (!!1)
                        , features = theFeatures global
                        , preprune = mkPreprune 0.3
                        , check    = \_ _ -> True 
                        , trainSettings = posTrainSettings }

leftCtx   = 2
rightCtx  = 1

maxSuffix = 7
maxPrefix = 5

type ET = E.EditTree String Char

theFeatures  ::    Conf
                -> LZipper [Smth ET] [Smth ET] [Smth ET]
                -> [Feature String Double]
theFeatures global tic = let prev = getSome  leftCtx   (left tic)
                         in
                              concatMap leftFeatures prev
                           ++ [Sym $ concat $ map getpos prev] -- concat prefix of previous poslabels
                           ++ focusFeatures     (focus tic)
                           ++ concatMap rightFeatures (getSome rightCtx   (right tic))
    where leftFeatures  (Just (Str form:Str label:ES script:_))
              = [ Sym $ label 
                , Sym $ apply script (low form)
                , Sym $ low form ]
          leftFeatures  Nothing                         = [Null , Null , Null]
          focusFeatures (Just (Str form:_))
                                                      = [ Sym $ low form 
                                                      , Sym $ spellingSpec form 
                                                      , lexmap (low form) 
                                                      , cluster form                                      
                                                      ]          
                                                      ++ prefixes maxPrefix (low form)
                                                      ++ suffixes maxSuffix (low form)
                                          --            ++ embedding mv  
          focusFeatures other = error $ "POS.theFeatures: format error"
          rightFeatures (Just (Str form:_)) = [Sym (low form),lexmap (low form)]
          rightFeatures Nothing     = [Null, Null]
          low = lowercase
          lexmap w = Set $ map snd $ Map.findWithDefault [] w (dictLex global)
          cluster w = case Map.lookup w (clusterDict global) of
                        Nothing -> Null
                        Just c  -> Sym c
          getpos Nothing = ""
          getpos (Just (Str form:Str label:_)) = head (splitPOS (lang global) label)
          embedding Nothing  = take 400 (repeat Null)
          embedding (Just v) = 
            let xs = U.toList v in [ maybe Null Num . lookup i $ xs  | i <- [0..399] ]
