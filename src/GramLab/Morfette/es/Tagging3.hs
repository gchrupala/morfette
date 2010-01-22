module GramLab.Morfette.ES.Tagging3 ( makeFeatureSpec )
where
import GramLab.Morfette.Features.TaggingUtil
import GramLab.Morfette.Features.LemmaUtil (applyScript,sesString)
import qualified GramLab.Morfette.Features.Common as C (winspec)
import Debug.Trace
import Data.Char
import Data.List (group)
import qualified Data.Map as Map
import GramLab.Morfette.ES.Lexicon (lexicon)
makeFeatureSpec params = FS { tokenClass     = mkTokenClass     params
                            , tokenFeatures  = mkTokenFeatures  params
                            , selectFeatures = mkSelectFeatures params 
                            , C.winspec      = winspec          params
                            }

--mkTokenClass _      tok | trace (show tok) False = undefined
mkTokenClass params (form,lemma,pos) = Combined { getSES = sesString form (fromMaybe form lemma) 
                                                , getPOS = fromMaybe "" pos }


mkTokenFeatures params tok@(form,_,_) =  feat_lform  : Set lexicon_mtags : (suffixes (maxSuffix params) lform)
    where feat_lform = if null form then Null else Sym lform
          lform      = lowercase form
          lexicon_mtags = map snd (Map.findWithDefault [] lform lexicon)

mkSelectFeatures params ix (E klass (lform: fs))
--    | ix < 0     = [Sym $ getPOS klass, mapSym (applyScript (getSES klass)) lform  ,lform]
--    | ix < 0     = [Sym $ getPOS klass, Sym . show . getSES $ klass ,lform]
    | ix < 0     = [Sym $ getPOS klass , mapSym (applyScript (getSES klass)) lform,lform]
    | ix > 0     = [lform]
    | otherwise  = lform: mapSym spellingSpec lform: fs

fromJust' (Just x) = x

spellingSpec x = map head . group . map collapse $ x

collapse c | isAlpha c && isUpper c = 'X'
           | isAlpha c && isLower c = 'x'
           | isDigit c              = '0'
           | c == '-'               = '-'
           | c == '_'               = '_'
           | otherwise              = '*'
