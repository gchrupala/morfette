{-# OPTIONS_GHC -fglasgow-exts #-}
module GramLab.Morfette.Utils ( train
                              , predict
                              , Flag(..)
                              , morfette
                              )
where
import Prelude hiding (print,getContents,putStrLn,putStr
                      ,writeFile,readFile)
import System.IO (stderr,stdout)
import System.IO.UTF8
import GramLab.Commands
import qualified GramLab.Morfette.Models as Models
import GramLab.Morfette.Models (Smth(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad hiding (join)
import GramLab.Utils (padRight,splitWith,splitOn
                     ,splitInto,join,tokenize,lowercase)
import qualified GramLab.Perceptron.Model as M
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
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified GramLab.Morfette.Config as C
import GramLab.Morfette.Evaluation
import GramLab.Morfette.Settings.Defaults
import GramLab.Intern (Table(..),intern,initial,runState,evalState)
import GramLab.FeatureSet (toFeatureSet,FeatureSet)
import qualified GramLab.Perceptron.Multiclass as Perceptron
import System.IO.Unsafe (unsafeInterleaveIO)

import Debug.Trace

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
          | EntropyTh Double
          | IterPOS Int
          | IterLemma Int
          | ModelId String
          deriving Eq

morfette fs fspecs = defaultMain (commands fs fspecs) "Usage: morfette command [OPTION...] [ARG...]"

commands fs fspecs = [
             ("train" , CommandSpec (train fs fspecs)
                          "train models"
                          [ Option [] ["dict-file"] 
                                       (ReqArg DictFile "PATH")
                                       "path to optional dictionary"
                          , Option [] ["language-configuration"] 
                                       (ReqArg Lang "es|pl|tr|..")
                                       "language configuration"
                          , Option [] ["iter-pos"] 
                                       (ReqArg (IterPOS . read) "NUM")
                                       "iterations for POS model"
                          , Option [] ["iter-lemma"] 
                                       (ReqArg (IterLemma . read) "NUM")
                                       "iterations for Lemma model"
                          ] 
              [ "TRAIN-FILE", "MODEL-DIR" ])
           , ("learn" , CommandSpec (learn fs fspecs)
                          "train models"
                          [ ]
              [ "MODEL-DIR" ])
           , ("extract-features", CommandSpec (extractFeatures fs fspecs)
                                "extract features"
                                [  Option [] ["dict-file"] 
                                       (ReqArg DictFile "PATH")
                                       "path to optional dictionary"
                                , Option [] ["model-id"]
                                       (ReqArg ModelId "pos|lemma")
                                       "model id (`pos' or `lemma')"
                                ]
              [ "MODEL-DIR"])
              
           , ("predict" , CommandSpec (predict fs fspecs)
                            "predict postags and lemmas using saved model data"
                            [ Option [] ["beam"]   
                                         (ReqArg (BeamSize . read) "+INT")
                                         "beam size to use"
                            , Option [] ["tokenize"] 
                                         (NoArg Tokenize)
                                         "tokenize input"
                            ] 
                            [ "MODEL-DIR" ] )
           , ("eval" , CommandSpec eval 
                          "evaluate morpho-tagging and lemmatization results"
                          [ Option [] ["ignore-case"] 
                                       (NoArg IgnoreCase)
                                       "ignore case for evaluation"
                          , Option [] ["baseline-file"] 
                                       (ReqArg BaselineFile "PATH")
                                      "path to baseline results"
                          , Option [] ["dict-file"] 
                                       (ReqArg DictFile "PATH")
                                       "path to optional dictionary"
                          , Option [] ["ignore-punctuation"] 
                                       (NoArg IgnorePunct)
                                       "ignore punctuation for evaluation"
                          , Option [] ["ignore-pos"] 
                                       (ReqArg IgnorePOS "POS-prefix")
                                       "ignore POS starting with POS-prefix for evaluation"
                          ]
                          ["TRAIN-FILE","GOLD-FILE","TEST-FILE"])
           ]


predict (_,format) fspecs flags [modelprefix] = do
  hPutStrLn stderr $ "Loading models from " ++ (modelFile modelprefix)
  ms <- fmap decode (B.readFile (modelFile modelprefix))
  ms == ms `seq` return ()
  when True $ do
    mwes <- loadMwes (mweFile modelprefix)
    lex        <- readConf (confFile modelprefix)
    txt <- getContents
    let models = zipWith Models.toModelFun (map ($lex) fspecs) ms
        defaultBeamSize = 3
        n = case [f | BeamSize f <- flags ] 
            of { [f] -> f ; _ -> defaultBeamSize }
        f = if Pipeline `elem` flags 
            then Models.predictPipeline
            else Models.predict
    putStr . unlines 
           . map format 
           . f n models 
           . toksToForms 
           . getToks flags mwes
           $ txt

confFile       dir = dir </> "conf.dat"
mweFile        dir = dir </> "mwe.dat" 
modelFile      dir = dir </> "models.dat"
classMapFile   dir = dir </> "classmap.dat"
featMapFile    dir = dir </> "featmap.dat"
lemmaModelFile dir = dir </> "lemma.dat"
posModelFile   dir = dir </> "pos.dat"
exampleFile    dir = dir </> "examples.dat"

defaultGaussianPrior = 1

extractFeatures  :: (Ord a,Binary a,Show a) =>
     (Token -> Models.Tok a, t)
     -> [Conf -> Models.FeatureSpec a]
     -> [Flag]
     -> [FilePath]
     -> IO ()
extractFeatures (prepr,fmt) [fspos,fslem] flags [modeldir] = do
  dict <- getDict flags Nothing
  --  dict == dict `seq` return ()
  let langConf = case [f | Lang f <- flags ] of { [] -> "xx" ; [f] -> f }
      lex = Conf { dictLex = dict
                 , lang = langConf }
      fs = case [f | ModelId f <- flags ] of
              ["pos"]   -> fspos lex
              ["lemma"] -> fslem lex
              []        -> fspos lex
              other -> error $ "GramLab.Morfette.Utils.extractFeatures: " 
                       ++ "invalid option value: " ++ show other
  toks <- fmap (map parseToken . lines) getContents
  let ws = filter (not . null) . map tokenForm $ toks
      (xm,ym,xys) =  convertFeatures
         . map swap
         . concatMap (Models.sentToExamples fs)
         . toksToSentences prepr 
         $ toks 
      
  createDirectoryIfMissing True modeldir
  putStr . unlines
             . map format
             . zip ws
             . map (\(x,y) -> (zip (IntMap.keys x) $ repeat (1::Float),y))
             $ xys
  saveConf (confFile modeldir) lex
  B.writeFile (classMapFile modeldir) . encode $ ym
  B.writeFile (featMapFile modeldir) . encode $ xm

format :: (String,([(Int,Float)],Int)) -> String
format (w,(x,y)) = unwords (w:show y : [ show i ++ ":" ++ show n 
                                       | (i,n) <- x ])
parse :: String -> (String,([(Int, Float)],Int))
parse s = case words s of
            (w:y:x) -> (w,( [ (read i,1) 
                                         | ixi <- x
                                       , let [i,xi] = splitOn ':' ixi
                                       ]
                     , read y ))
swap (y,x) = (x,y)

convertFeatures xys = 
    let (xs,ys) = unzip xys
        (xs',xm) = flip runState initial . mapM toFeatureSet $ xs
        (ys',ym) = flip runState initial . mapM intern $ ys
    in (xm,ym,zip xs' ys')


learn (prepr,_) fspecs flags [modeldir] = do
  T hi_c cm <- fmap decode (B.readFile $ classMapFile modeldir) 
              :: IO (Table (Smth String))

  T hi_f _ <- fmap decode (B.readFile $ featMapFile modeldir)
             :: IO (Table (Int, Maybe String))
  (ws,xys) <- fmap (unzip 
                    . concat 
                    . take 3 
                    . repeat 
                    . map parse 
                    . lines ) 
              getContents
  let rate = 0.1
      checker = \key y -> True
      bounds = ((0,0),(hi_c-1,hi_f-1))
      span = 168803
  let model = Perceptron.learn rate bounds span  ws xys
  B.writeFile (posModelFile modeldir) . encode $ model


train  :: (Ord a, Show a, Binary a) =>
     (Token -> Models.Tok a, t)
     -> [Conf -> Models.FeatureSpec a]
     -> [Flag]
     -> [FilePath]
     -> IO ()
train (prepr,_) fspecs flags [dat,modeldir] = do
  toks <- fmap (map parseToken . lines) (readFile dat)
  let tokSet = Set.fromList [ s | t@(s,_,_) <- toks ]
  dict <- getDict flags $ Just tokSet
  let langConf = case [f | Lang f <- flags ] of { [] -> "xx" ; [f] -> f }
      lex = Conf { dictLex = dict
                 , lang = langConf }
      mwes = mweSet toks
      g = case [f | EntropyTh f <- flags ] of 
            [] -> M.entropyTh posTrainSettings  
            [f] -> f 
      i_p = case [f | IterPOS f <- flags ] of  
              [] ->  M.iter posTrainSettings 
              [f] -> f 
      i_l = case [f | IterLemma f <- flags ] of  
              [] ->  M.iter lemmaTrainSettings 
              [f] -> f 
      sentences = toksToSentences prepr toks
  createDirectoryIfMissing True modeldir
  let models = Models.train (map (\(i,fs) -> 
                                      let fs' = fs lex
                                          ts = Models.trainSettings fs'
                                      in fs' { Models.trainSettings = 
                                                   ts { M.entropyTh = g 
                                                      , M.iter = i
                                                      } })
                             $ zip [i_p,i_l] fspecs) 
            $ sentences
  B.writeFile (modelFile modeldir) (encode models)
  saveConf (confFile modeldir) lex
  saveMwes (mweFile modeldir) mwes

toksToSentences :: (Token -> Models.Tok a) -> [Token] -> [[Models.Tok a]]
toksToSentences f toks = map (map f)  $ splitWith isNullToken toks

toksToForms :: [Token] -> [[Models.Tok a]]
toksToForms toks = map (map (\ (f,_,_) ->[Str f])) 
                   . splitWith isNullToken 
                   $ toks

parseSents :: String -> [[Models.Tok a]]
parseSents  = splitWith null . map (map Str) . map words . lines

getDict :: [Flag] -> Maybe (Set.Set String) -> IO Lexicon
getDict flags tokSet = do
  case [f | DictFile f <- flags ] of
    [f] -> do 
            d <- fmap (parseLexicon tokSet)  $ readFile f
            return d
    [] -> return emptyLexicon

getToks :: [Flag] -> [[String]] -> String -> [Token]
getToks flags mwes text = 
    let f = if Tokenize `elem` flags 
            then  concatMap (detectMwes mwes) 
                  . List.intersperse [""] 
                  . map tokenize 
            else id
    in map parseToken . f . lines $ text
             

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
  (train,gold,test,baseline) <- fmap (\ (tr, g, t, b) -> (tr,toks g, toks t, fmap toks b)) (getEval flags trainf goldf testf)
  let seen = Set.fromList (map tokenForm train)
  dict  <- getDict flags . Just $ seen
  let isUnseen (form,_,_) = not (form `Set.member` seen)
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
-- FIXME Sentence accuracy is broken
--  putStrLn $ "Sentence accuracy:\n" ++ showAccuracy sent_acc
  where toks  xs   = filter (not . isNullToken) xs
        sents xs   = splitWith isNullToken xs
    
filterZip :: [Bool] -> [a] -> [a]
filterZip xs ys = catMaybes $ zipWith (\b x -> if b then Just x else Nothing) xs ys
    
