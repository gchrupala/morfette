{-# LANGUAGE NoMonomorphismRestriction #-}
module GramLab.Data.Diff.EditTree2Rev ( make
                                      , apply
                                      , size
                                      , check
                                      , EditTree(..)
                                      )
where

import qualified GramLab.Data.Diff.EditTree2 as ET2

newtype EditTree s a = ETR (ET2.EditTree s a) deriving (Show,Eq,Ord)

make xs ys = ETR (ET2.make (reverse xs) (reverse ys))
apply (ETR s) xs = reverse $ ET2.apply s (reverse xs)
size (ETR s) = ET2.size s
check (ETR s) xs = ET2.check s (reverse xs)
