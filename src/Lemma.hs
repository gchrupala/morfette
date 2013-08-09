module Lemma (featureSpec,apply,make)
where
import qualified Data.Map as Map
import GramLab.Morfette.Features.Common
import qualified GramLab.Data.Diff.EditTree as E
import GramLab.Morfette.Models2 (Row(..))
import GramLab.Morfette.Utils (ROW, Input(..), Output(..))
import Debug.Trace
import Data.Maybe (fromMaybe)

featureSpec global  = FS { label    = theLabel
                         , features = theFeatures  global
                         , preprune = mkPreprune 0.3
                         , check    = theCheck 
                         , trainSettings = lemmaTrainSettings }

theCheck :: LZipper ROW ROW ROW -> Output -> Bool
theCheck z (ET s) = 
  let wf =  prepare 
          . getForm 
          . fromMaybe (error "Lemma.featureSpec.check: Nothing") 
          . focus
          $ z
  in E.check s wf && not (null (E.apply s wf))
     
theLabel :: ROW ->  Output
theLabel r = case output r of
  [POS _, et@(ET _)] -> et
  other -> error $ "Lemma.theLabel: match failed with " ++ show other

getForm :: ROW -> String
getForm (Row { input = Input { inputForm = form } }) = form 

maxSuffix = 7
maxPrefix = 5

prepare = lowercase

type ET = E.EditTree String Char

theFeatures  ::    Conf
                -> LZipper ROW ROW ROW
                -> [Feature String Double]
theFeatures  global tic = focusFeatures  (focus tic)
    where focusFeatures (Just (Row { input = Input { inputForm = form } 
                                   , output = (POS label:_) })) = 
            [ Sym $ low form, Sym $ label
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
make form lemma = ET $ E.make (prepare form) (prepare lemma)