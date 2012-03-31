module Lemma (featureSpec,apply,make)
where
import qualified Data.Map as Map
import GramLab.Morfette.Features.Common
import qualified GramLab.Data.Diff.EditTree as E
import Debug.Trace
import Data.Maybe (fromMaybe)
featureSpec global  = FS { label    = theLabel
                         , features = theFeatures  global
                         , preprune = mkPreprune 0.3
                         , check    = theCheck 
                         , trainSettings = lemmaTrainSettings }

theCheck z (ES s) = 
  let wf =  prepare 
          . getForm 
          . fromMaybe (error "Lemma.featureSpec.check: Nothing") 
          . focus
          $ z
  in E.check s wf && not (null (E.apply s wf))
     
theLabel (Str form:Str pos:Str lemma:_) = make form lemma
theLabel (Str _   :Str pos:ES  s:_)     = ES s
theLabel other = error $ "Lemma.theLabel: match failed with " ++ show other

getForm =  str . head

maxSuffix = 7
maxPrefix = 5

prepare = lowercase

theFeatures  global tic = focusFeatures     (focus tic)
    where focusFeatures (Just (Str form:Str label:_)) = [ Sym $ low form, Sym $ label
                                                        , Sym $ spellingSpec form
                                                        , lexmap (low form)
                                                        , cluster form 
                                                ]
                                              ++ prefixes maxPrefix (low form)
                                              ++ suffixes maxSuffix (low form)
          focusFeatures other = error $ "Lemma.theFeatures: " ++ show other
          low = lowercase
          lexmap w = Set $ map (show . make w . fst) $ Map.findWithDefault [] w (dictLex global)
          cluster w  = case Map.lookup w (clusterDict global) of
                          Nothing -> Null
                          Just c  -> Sym c

decode str = case reads str of
               [(s,"")] -> s
               other    -> error ("Lemma.decode: no parse: " ++ str)
apply s str = E.apply s (prepare str)
make form lemma = ES $ E.make (prepare form) (prepare lemma)