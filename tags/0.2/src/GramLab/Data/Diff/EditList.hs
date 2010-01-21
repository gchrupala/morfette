{-# LANGUAGE NoMonomorphismRestriction #-}
module GramLab.Data.Diff.EditList ( make
                                  , apply
                                  , check
                                  , EditList
                                  )
where
import GramLab.Data.LCS.SimpleMemo (ses, check, apply, EditScript(..)) 
type EditList a = EditScript a
make = ses
