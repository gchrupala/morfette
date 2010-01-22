{-# OPTIONS_GHC -fglasgow-exts #-}
module GramLab.Morfette.Utils ( train
                              , predict
                              , Flag(..)
                              , morfette
                              )
where
import Prelude hiding (getContents,putStrLn,writeFile,readFile)
import System.IO (stderr,stdout)
import System.IO.UTF8
import GramLab.Commands
import qualified GramLab.Morfette.Models as Models
import GramLab.Morfette.Models (Smth(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad hiding (join)
import GramLab.Utils (padRight,splitWith,splitInto,join,tokenize,lowercase)

import qualified GramLab.Maxent.ZhangLe.Model    as M
import GramLab.Morfette.Token
import GramLab.Morfette.LZipper
import GramLab.Morfette.MWE
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Printf
import qualified Data.List as List
import Data.Char
import GramLab.Morfette.Lang.Conf
import GramLab.Morfette.BinaryInstances
import Data.Binary.Strict
import qualified Data.ByteString.Lazy as B
import qualified GramLab.Morfette.Config as C
import GramLab.Morfette.Evaluation

data Flag = ModelPrefix String
          | Eval
          | BeamSize Int 
          | IgnoreCase
          | DictFile FilePath
          | BaselineFile FilePath
          | Lang Lang
          | Gaussian Double
          | Tokenize 
          | IgnorePunct
          | IgnorePOS String
          | Pipeline
          deriving Eq

morfette fs fspecs = defaultMain (commands fs fspecs) "Usage: morfette command [OPTION...] [ARG...]"

commands fs fspecs = [
             ("train" , CommandSpec (train fs fspecs)
                          "train maxent model"
                          [  Option [] ["dict-file"] (ReqArg DictFile "PATH")
                                       "path to optional dictionary"
                          , Option [] ["language-configuration"] (ReqArg Lang "es|pl|tr|..")
                                  "language configuration"
                          , Option [] ["gaussian-prior"] (ReqArg (Gaussian . read) "NUM")
                                   "gaussian-prior"
                          ] 
                          [ "TRAIN-FILE", "MODEL-DIR" ])
              
           , ("predict" , CommandSpec (predict fs fspecs)
                            "predict postags and lemmas using saved model data"
                            [ Option [] ["beam"]   (ReqArg (BeamSize . read) "+INT")
                                         "beam size to use"
                            , Option [] ["tokenize"] (NoArg Tokenize)
                                         "tokenize input"
                            , Option [] ["pipeline"] (NoArg Pipeline)
                                         "use pipeline model"
                            ] 
                            [ "MODEL-DIR" ] )
           , ("eval" , CommandSpec eval 
                          "evaluate morpho-tagging and lemmatization results"
                          [ Option [] ["ignore-case"] (NoArg IgnoreCase)
                                       "ignore case for evaluation"
                          , Option [] ["baseline-file"] (ReqArg BaselineFile "PATH")
                                      "path to baseline results"
                          , Option [] ["dict-file"] (ReqArg DictFile "PATH")
                                       "path to optional dictionary"
                          , Option [] ["ignore-punctuation"] (NoArg IgnorePunct)
                                       "ignore punctuation for evaluation"
                          , Option [] ["ignore-pos"] (ReqArg IgnorePOS "POS-prefix")
                                       "ignore POS starting with POS-prefix for evaluation"
                          ]
                          ["TRAIN-FILE","GOLD-FILE","TEST-FILE"])
           ]


predict (_,format) fspecs flags [modelprefix] = do
  mwes <- loadMwes (mweFile modelprefix)
  toks <- fmap (getToks flags mwes) getContents
  lex        <- readConf (confFile modelprefix)
  ms <- fmap decode (B.readFile (modelFile modelprefix))
  let models = zipWith Models.toModelFun (map ($lex) fspecs) ms
      sentences = toksToForms toks
      defaultBeamSize = 3
      n = case [f | BeamSize f <- flags ] of { [f] -> f ; _ -> defaultBeamSize }
      predictions = if Pipeline `elem` flags 
                    then Models.predictPipeline n models sentences
                    else Models.predict  n models sentences
  putStrLn (join "\n" (map format predictions))

confFile       dir = dir </> "conf.model"
mweFile        dir = dir </> "mwe.model" 
modelFile      dir = dir </> "models.model"
defaultGaussianPrior = 1

train (prepr,_) fspecs flags [dat,modeldir] = do
  toks <- fmap (map parseToken . lines) (readFile dat)
  dict <- getDict flags        
  let langConf = case [f | Lang f <- flags ] of { [] -> "xx" ; [f] -> f }
      lex = Conf { dictLex = dict, trainLex = toksToLexicon toks, lang = langConf }
      mwes = mweSet toks
      g = case [f | Gaussian f <- flags ] of { [] -> defaultGaussianPrior ; [f] -> f }
      sentences = toksToSentences prepr toks
  createDirectoryIfMissing True modeldir
  models <- Models.train (map (\fs -> let fs' = fs lex
                                          ts = Models.trainSettings fs'
                                      in fs' { Models.trainSettings = ts { M.gaussian = g } })
                          fspecs) 
            $ sentences
  B.writeFile (modelFile modeldir) (encode models)
  saveConf (confFile modeldir) lex
  saveMwes (mweFile modeldir) mwes

toksToSentences :: (Token -> Models.Tok a) -> [Token] -> [[Models.Tok a]]
toksToSentences f toks = map (map f)  $ splitWith isNullToken toks

toksToForms :: [Token] -> [[Models.Tok a]]
toksToForms toks = map (map (\ (f,_,_) ->[Str f]))  $ splitWith isNullToken toks

parseSents :: String -> [[Models.Tok a]]
parseSents  = splitWith null . map (map Str) . map words . lines

getDict :: [Flag] -> IO Lexicon
getDict flags = do
  case [f | DictFile f <- flags ] of
    [f] -> do 
            text <- readFile f
            return (parseLexicon text)
    [] -> return emptyLexicon

getToks :: [Flag] -> [[String]] -> String -> [Token]
getToks flags mwes text = 
    let ls = lines text
    in if Tokenize `elem` flags
          then map parseToken 
                   . concatMap (detectMwes mwes) 
                   . List.intersperse [""] 
                   . map tokenize $ ls 
       else map parseToken ls

             

formatTriple (form,lemma,pos) = unwords . map (padRight ' ' 12) $ [form,lemma,pos] 
formatToken (f,ml,mp) = unwords [f,fromMaybe "" ml,fromMaybe "" mp]




getEval flags trainf goldf testf = do
  let uncase = if IgnoreCase `elem` flags then
                   map (\(form,lemma,pos) -> (lowercase form,fmap lowercase lemma, fmap lowercase pos))
               else id
      ignore = case [f | IgnorePOS f <- flags ] of
                 [] -> const False
                 xs -> (\(_,_,mpos) -> case mpos of
                                         Nothing -> False 
                                         Just pos -> any (`List.isPrefixOf` pos) xs)
      isPunct = if IgnorePunct `elem` flags then (\t@(form,_,_) -> 
                                                      (not . isNullToken) t && all isPunctuation form) else const False
      keep tok = (not . ignore) tok && (not . isPunct) tok
  train <- fmap uncase (getTokens trainf)
  gold  <- fmap uncase (getTokens goldf)
  test  <- fmap uncase (getTokens testf)
  baseline <- case [f | BaselineFile f <- flags ] of 
                [] -> return Nothing
                [f] -> fmap (Just . uncase) (getTokens f)
  let keeps = map keep gold
  return ( train
         , filterZip keeps gold
         , filterZip keeps test
         , fmap (filterZip keeps) baseline )
 where getTokens f = fmap (map parseToken . lines) (readFile f)
-- FIXME its breaks sentence accuracy somehow...

eval flags [trainf,goldf,testf] = do
  dict  <- getDict flags
  (train,gold,test,baseline) <- fmap (\ (tr, g, t, b) -> (tr,toks g, toks t, fmap toks b)) (getEval flags trainf goldf testf)
  let seen = Set.fromList (map tokenForm train)
      isUnseen (form,_,_) = not (form `Set.member` seen)
      isUnseenInDict (form,_,_) = not (lowercase form `Map.member` dict)
      isUnseenBoth x = isUnseen x && isUnseenInDict x
      all_acc    = tokenAccuracy gold test baseline
      unseen_acc = tokenAccuracy (filter isUnseen gold) (filter isUnseen test) 
                   (fmap (filter isUnseen) baseline)
      seen_acc   = tokenAccuracy (filter (not . isUnseen) gold) (filter (not . isUnseen) test)
                   (fmap (filter (not . isUnseen)) baseline)
      unseen_ratio = 100 * fromIntegral (length (filter isUnseen test)) / fromIntegral (length test)
      unseen_train_and_dict_acc = tokenAccuracy (filter isUnseenBoth gold)
                                                (filter isUnseenBoth test)
                                                (fmap (filter isUnseenBoth) baseline)
      sent_acc   = sentenceAccuracy (sents gold) (sents test) (fmap sents baseline)
      goldlemma g= map (lowercase . tokenLemma) g
      uniquePOS  = fromIntegral $ Set.size $ Set.fromList $ map tokenPOS gold
  putStrLn $ "Unseen word ratio: " ++ printf "%4.2f" (unseen_ratio::Double)
  putStrLn $ "Token accuracy all:\n" ++ showAccuracy all_acc
  putStrLn $ "Token accuracy seen:\n" ++ showAccuracy seen_acc
  putStrLn $ "Token accuracy unseen:\n" ++ showAccuracy unseen_acc
  when (Map.size dict > 0) 
       (putStrLn $ "Token accuracy unseen train+dict:\n" ++ showAccuracy unseen_train_and_dict_acc)
  putStrLn $ "Sentence accuracy:\n" ++ showAccuracy sent_acc
  where toks  xs   = filter (not . isNullToken) xs
        sents xs   = splitWith isNullToken xs
    
filterZip :: [Bool] -> [a] -> [a]
filterZip xs ys = catMaybes $ zipWith (\b x -> if b then Just x else Nothing) xs ys
    
