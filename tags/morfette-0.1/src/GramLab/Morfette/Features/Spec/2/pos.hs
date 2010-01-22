module POS (featureSpec)
where
import GramLab.Morfette.Features.POS


featureSpec apply = FS { label    = toLabel . fromJust .  tokenPOS
                       , features = theFeatures apply }
leftCtx   = 2
rightCtx  = 1

maxSuffix = 7
maxPrefix = 5


theFeatures apply tic =       concatMap leftFeatures  (getSome  leftCtx   (left tic))
                           ++ focusFeatures     (focus tic)
                           ++ concatMap rightFeatures (getSome rightCtx   (right tic))
    where leftFeatures  (Just (form,label)) = [ Sym $ getLeft label 
                                              , Sym $ apply (getRight label) (low form)
                                              , Sym (low form)]
          leftFeatures  Nothing                         = [Null , Null , Null]
          focusFeatures (Just form)         = [Sym $ low form, Sym $ spellingSpec form ]
                                                          ++ prefixes maxPrefix (low form)
                                                          ++ suffixes maxSuffix (low form)
          rightFeatures (Just form) = [Sym (low form)]
          rightFeatures Nothing     = [Null]
          low = lowercase


