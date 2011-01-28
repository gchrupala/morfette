{-# OPTIONS_GHC -fglasgow-exts #-}
module GramLab.Morfette.Features.Common ( spellingSpec
                                        , prefixes
                                        , suffixes
                                        , lowercase
                                        , mapSym
                                        , mapNum
                                        , getSome
                                        , module GramLab.Morfette.Models
                                        , module GramLab.Morfette.Settings.Defaults
                                        , module GramLab.Morfette.LZipper
                                        , module GramLab.FeatureSet
                                        , module GramLab.Morfette.Lang.Conf
                                        )

where
import GramLab.Morfette.Token 
import GramLab.Morfette.LZipper(LZipper)
import GramLab.FeatureSet
import Data.Char
import Data.List
import Aux.Utils (padRight)
import Data.Binary
import Control.Monad (liftM,liftM2)
import GramLab.Morfette.Settings.Defaults
import GramLab.Morfette.Models
import GramLab.Morfette.LZipper
import GramLab.Morfette.Lang.Conf

mapSym f (Sym s) = Sym (f s)
mapSym _ x       = x
mapNum f (Num n) = Num (f n)
mapNum _ x       = x

lowercase = map toLower

suffixes size = padRight Null size . map Sym . take size . drop 1 . reverse . tails
prefixes size = padRight Null size . map Sym . take size . drop 1 .           inits
bigrams [] = []
bigrams xs = zipWith ((. return) . (:)) xs (tail xs)
getSome size xs = take size  . padRight Nothing size . map Just $ xs
spellingSpec x = map head . group . map collapse $ x

collapse c | isAlpha c && isUpper c = 'X'
           | isAlpha c && isLower c = 'x'
           | isDigit c              = '0'
           | c == '-'               = '-'
           | c == '_'               = '_'
           | otherwise              = '*'
