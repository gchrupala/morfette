module POS (featureSpec)
where
import GramLab.Morfette.Features.Common
import qualified GramLab.Data.Diff.EditTree as E
import GramLab.Morfette.Utils (ROW, Input(..), Output(..))
import GramLab.Morfette.Models2 (Row(..))
import qualified Data.Map as Map
import Lemma (apply)
import qualified Data.Vector.Unboxed as U

featureSpec global = FS { label    = theLabel
                        , features = theFeatures global
                        , preprune = mkPreprune 0.3
                        , check    = \_ _ -> True 
                        , trainSettings = posTrainSettings }

leftCtx   = 2
rightCtx  = 1

maxSuffix = 7
maxPrefix = 5

type ET = E.EditTree String Char

theLabel :: ROW -> Output
theLabel (Row { output = (pos@(POS _):_) }) = pos
theLabel other = error $ "POS.theLabel failed with " ++ show other
  
theFeatures  ::    Conf
                -> LZipper ROW ROW ROW
                -> [Feature String Double]
theFeatures global tic = let prev = getSome  leftCtx   (left tic)
                         in
                              concatMap leftFeatures prev
                           ++ [Sym $ concat $ map getpos prev] -- concat prefix of previous poslabels
                           ++ focusFeatures     (focus tic)
                           ++ concatMap rightFeatures (getSome rightCtx   (right tic))
    where leftFeatures  (Just (Row { input  = Input { inputForm = form }
                                   , output = [POS label, ET script] })) =
                [ Sym $ label 
                , Sym $ apply script (low form)
                , Sym $ low form ]
          leftFeatures  Nothing                         = [Null , Null , Null]
          focusFeatures (Just (Row { input  = Input { inputForm = form , inputEmb = mv } } ))
                                                      = [ Sym $ low form 
                                                      , Sym $ spellingSpec form 
                                                      , lexmap (low form) 
                                                      , cluster form                                      
                                                      ]          
                                                      ++ prefixes maxPrefix (low form)
                                                      ++ suffixes maxSuffix (low form)
                                                      ++ embedding mv  
          focusFeatures other = error $ "POS.theFeatures: format error"
          rightFeatures (Just (Row { input  = Input { inputForm = form } })) = 
                                      [Sym (low form),lexmap (low form)]
          rightFeatures Nothing     = [Null, Null]
          low = lowercase
          lexmap w = Set $ map snd $ Map.findWithDefault [] w (dictLex global)
          cluster w = case Map.lookup w (clusterDict global) of
                        Nothing -> Null
                        Just c  -> Sym c
          getpos Nothing = ""
          getpos (Just (Row { output = (POS label:_) })) = head (splitPOS (lang global) label)
          embedding Nothing  = []
          embedding (Just v) = [ Num n | n <- U.toList v ]

