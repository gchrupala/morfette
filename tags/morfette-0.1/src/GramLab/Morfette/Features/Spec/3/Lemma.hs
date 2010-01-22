module Lemma (featureSpec)
where
import GramLab.Morfette.Features.Lemma
import qualified GramLab.Data.Diff.EditListBidi as E

featureSpec  = FS { label    = \(form,Just lemma,_) -> RHS $ E.make  (prepare form) (prepare lemma)
                  , features = theFeatures  
                  , apply    = \et form -> E.apply et (prepare form) 
                  , check    = \et form -> E.check et (prepare form) 
                  }


leftCtx   = 2
rightCtx  = 1

maxSuffix = 7
maxPrefix = 5

prepare = lowercase

theFeatures  tic =            concatMap leftFeatures  (getSome  leftCtx   (left tic))
                           ++ focusFeatures     (focus tic)
                           ++ concatMap rightFeatures (getSome rightCtx   (right tic))
    where leftFeatures  (Just (form,label)) = [ Sym $ getLeft label 
                                              , Sym $ E.apply (getRight label) (low form)
                                              , Sym (low form)]
          leftFeatures  Nothing             = [Null , Null , Null]
          focusFeatures (Just (form,label)) = [Sym $ low form, Sym $ spellingSpec form, Sym $ getLeft label]
                                              ++ prefixes maxPrefix (low form)
                                              ++ suffixes maxSuffix (low form)
          rightFeatures (Just form)         = [Sym (low form)]
          rightFeatures Nothing             = [Null]
          low = lowercase


