module Lemma (featureSpec)
where
import GramLab.Morfette.Features.Lemma
import qualified GramLab.Data.Diff.EditTree as ET

featureSpec  = FS { label    = \(form,Just lemma,_) -> RHS $ ET.make  (prepare form) (prepare lemma)
                  , features = theFeatures  
                  , apply    = \et form -> ET.apply et (prepare form) 
                  , check    = \et form -> ET.check et (prepare form) 
                  }


leftCtx   = 2
rightCtx  = 1

maxSuffix = 7
maxPrefix = 5

prepare = lowercase

type LemmaClass = ET.EditTree Char

theFeatures :: LZipper (Form, OR String LemmaClass) (Form, OR String LemmaClass) Form
            -> [Feature String Double]
theFeatures  tic =            concatMap leftFeatures  (getSome  leftCtx   (left tic))
                           ++ focusFeatures     (focus tic)
                           ++ concatMap rightFeatures (getSome rightCtx   (right tic))
    where leftFeatures  (Just (form,label)) = [ Sym $ getLeft label 
                                              , Sym $ ET.apply (getRight label) (low form)
                                              , Sym (low form)]
          leftFeatures  Nothing             = [Null , Null , Null]
          focusFeatures (Just (form,label)) = [Sym $ low form, Sym $ spellingSpec form, Sym $ getLeft label]
                                              ++ prefixes maxPrefix (low form)
                                              ++ suffixes maxSuffix (low form)
          rightFeatures (Just form)         = [Sym (low form)]
          rightFeatures Nothing             = [Null]
          low = lowercase


