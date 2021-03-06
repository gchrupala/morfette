module GramLab.Morfette.Utils ( train
                              , predict
                              , Flag(..)
                              , Input (..)
                              , Output (..)
                              , ROW
                              , morfette
                              )
where
import Prelude hiding (print,getContents,putStrLn,putStr
                      ,writeFile,readFile)
import System.IO (stderr,stdout,stdin,hSetBinaryMode)
import System.IO.UTF8 hiding (getContents,print,putStr,putStrLn)
import qualified System.IO.UTF8 as UTF8
import GramLab.Commands
import qualified GramLab.Morfette.Models2 as Models
import GramLab.Morfette.Models2 (Row(..))
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
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import qualified GramLab.Morfette.Config as C
import GramLab.Morfette.Evaluation
import GramLab.Morfette.Settings.Defaults
import GramLab.Intern (Table(..),intern,initial,runState,evalState)
import GramLab.FeatureSet (toFeatureSet,FeatureSet)
import GramLab.Data.Diff.EditTree (EditTree)
import qualified Paths_morfette
import Data.Version (Version(..))
import Debug.Trace

data Flag = ModelPrefix String
          | Eval
          | BeamSize Int 
          | Multi Int
          | IgnoreCase
          | DictFile FilePath
          | ClusterFile FilePath
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
          | PruneUniqLabels
          deriving Eq

data Input  = Input { inputForm :: !String, inputEmb :: !(Maybe Emb) }
              deriving (Eq, Ord, Show)
                       
data Output = ET !(EditTree String Char) | POS !String 
            deriving (Eq, Ord, Show)

instance Binary Output where
  put (ET et) = put False >> put et
  put (POS pos) = put True >> put pos
  get = do
    tag <- get
    case tag of
      False -> liftM ET get
      True -> liftM POS get

type ROW = Row Input Output
type FEATSPEC = Models.FeatureSpec Input Output

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
                          , Option [] ["prune-unique-labels"] 
                                      (NoArg PruneUniqLabels)
                                       "discard labels occurring only once"
                        {-  , Option [] ["cluster-file"]
                                       (ReqArg ClusterFile "PATH")
                                        "path to optional cluster file"
                        -}
                          ] 
              [ "TRAIN-FILE", "MODEL-DIR" ])
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
                            , Option [] ["multi"]
                                         (ReqArg (Multi . read) "+INT")
                                         "n-best output"
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
           , ("version", CommandSpec version "show version" [] [] )
                       
           ]

version _ _ = do
  let showVersion (Version { versionBranch = is })  = 
          "morfette-" ++ (List.concat . List.intersperse "." . map show $ is)
  putStrLn . showVersion $ Paths_morfette.version
    

predict (_,format) fspecs flags [modelprefix] = do
  hPutStrLn stderr $ "Loading models from " ++ (modelFile modelprefix)
  ms <- fmap decode (B.readFile (modelFile modelprefix))
  ms == ms `seq` return ()
  when True $ do
    mwes <- loadMwes (mweFile modelprefix)
    lex        <- readConf (confFile modelprefix)
    txt <- Text.getContents
    let models = zipWith Models.toModelFun (map ($lex) fspecs) ms
        defaultBeamSize = 3
        n = case [f | BeamSize f <- flags ] 
            of { [f] -> f ; _ -> defaultBeamSize }
        k = case [f | Multi f <- flags ]
            of { [f] -> f ; _ -> 1 }
        f = if Pipeline `elem` flags 
            then if k /= 1 
                 then error $  "GramLab.Morfette.Utils.predict: " 
                            ++ "pipeline mode doesn't work with n-best output"
                 else \ n ms -> map (map return) 
                                  . Models.predictPipeline n ms 
            else Models.predict k
    putStr . unlines 
           . map format 
           . f n models 
           . toksToForms 
           . getToks flags mwes
           $ txt
predict _fs _fspecs _flags _args = do
  error $ "GramLab.Morfette.Utils.predict: " 
        ++ "Incorrect arguments to command predict."

confFile       dir = dir </> "conf.model"
mweFile        dir = dir </> "mwe.model" 
modelFile      dir = dir </> "models.model"
classMapFile   dir = dir </> "classmap.model"
featMapFile    dir = dir </> "featmap.model"

defaultGaussianPrior = 1

extractFeatures  ::      
        (Token -> ROW, t)
     -> [Conf -> FEATSPEC]
     -> [Flag]
     -> [FilePath]
     -> IO ()
extractFeatures (prepr,fmt) [fspos,fslem] flags [modeldir] = do
  dict <- getDict flags Nothing
  --  dict == dict `seq` return ()
  clusters <- getClusters flags
  let langConf = case [f | Lang f <- flags ] of { [] -> "xx" ; [f] -> f }
      lex = Conf { dictLex = dict
                 , clusterDict = clusters
                 , lang = langConf }
      fs = case [f | ModelId f <- flags ] of
              ["pos"]   -> fspos lex
              ["lemma"] -> fslem lex
              []        -> fspos lex
              other -> error $ "GramLab.Morfette.Utils.extractFeatures: " 
                       ++ "invalid option value: " ++ show other
  toks <- fmap (map parseToken . Text.lines) Text.getContents
  let ws = filter (not . null) . map tokenForm $ toks
      (xm,ym,xys) =  convertFeatures
         . map swap
         . concatMap (Models.sentToExamples fs)
         . toksToSentences prepr 
         $ toks 
  putStr . unlines 
             . map format
             . zip ws
             $ xys
  B.writeFile (classMapFile modeldir) . encode $ ym
  B.writeFile (featMapFile modeldir) . encode $ xm
extractFeatures _fs _fspecs _flags _args = do
  error $ "GramLab.Morfette.Utils.extractFeatures: " 
        ++ "Incorrect arguments to command extract-features."
        
format :: (String,(IntMap.IntMap Double,Int)) -> String
format (w,(x,y)) = unwords (w:show y : [ show i ++ ":" ++ show n 
                                       | (i,n) <- IntMap.toList x ])
parse :: String -> (String,(IntMap.IntMap Double,Int))
parse s = case words s of
            (w:y:x) -> (w,( IntMap.fromList [ (read i,read xi) 
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


train  :: 
     (Token -> ROW, t)
     -> [Conf -> FEATSPEC]
     -> [Flag]
     -> [FilePath]
     -> IO ()
train (prepr,_) fspecs flags [dat,modeldir] = do
  toks <- fmap (map parseToken . Text.lines) (Text.readFile dat)
  let tokSet = Set.fromList [ tokenForm t | t <- toks ]
  dict <- getDict flags Nothing
  clusters <- getClusters flags
  let langConf = case [f | Lang f <- flags ] of { [] -> "xx" ; [f] -> f }
      lex = Conf { dictLex = dict
                 , clusterDict = clusters
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
                                                      }
                                             , Models.pruneUniqLabels = PruneUniqLabels `elem` flags
                                               })
                             $ zip [i_p,i_l] fspecs) 
            $ sentences
  B.writeFile (modelFile modeldir) (encode models)
  saveConf (confFile modeldir) lex
  saveMwes (mweFile modeldir) mwes
train _fs _fspecs _flags _args = do
  error $ "GramLab.Morfette.Utils.train: " 
        ++ "Incorrect arguments to command train."
        
toksToSentences :: (Token -> ROW) -> [Token] -> [[ROW]]
toksToSentences f toks = map (map f)  $ splitWith isNullToken toks

-- Transform list of tokens to list of sentences, where each sentence is a list of ROWs 
-- (with empty output fields)
toksToForms :: [Token] -> [[ROW]]
toksToForms toks =   
  [ [  Row { input = Input { inputForm = tokenForm tok ,
                             inputEmb  = tokenEmb tok }
           , output = [] } 
       | tok <- sent ] 
  | sent <-  splitWith isNullToken toks ]
  
getDict :: [Flag] -> Maybe (Set.Set String) -> IO Lexicon
getDict flags tokSet = do
  case [f | DictFile f <- flags ] of
    [f] -> fmap (parseLexicon tokSet)  $ readFile f
    [] -> return emptyLexicon

getClusters :: [Flag] -> IO ClusterDict
getClusters flags = do
  case [f | ClusterFile f <- flags ] of
    [f] -> fmap parseClusterDict $ readFile f
    [] -> return Map.empty

getToks :: [Flag] -> [[String]] -> Text.Text -> [Token]
getToks flags mwes text = 
    let f = if Tokenize `elem` flags 
            then  map Text.pack 
                  . concatMap (detectMwes mwes) 
                  . List.intersperse [""] 
                  . map tokenize 
                  . map Text.unpack
            else id
    in map parseToken . f . Text.lines $ text
             
-- TODO remove these if unnecessary
formatTriple (form,lemma,pos) = unwords . map (padRight ' ' 12) $ [form,lemma,pos] 
formatToken (f,ml,mp) = unwords [f,fromMaybe "" ml,fromMaybe "" mp]

getEval flags trainf goldf testf = do
  let uncase = if IgnoreCase `elem` flags then
                   map lowercaseToken 
               else id
      ignore = case [f | IgnorePOS f <- flags ] of
                 [] -> const False
                 xs -> (\t -> case tokenPOS t of
                           Nothing -> False 
                           Just pos -> any (`List.isPrefixOf` pos) xs)
      isPunct = if IgnorePunct `elem` flags 
                then (\t -> (not . isNullToken) t && all isPunctuation (tokenForm t)) 
                else const False
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
 where getTokens f = fmap (map parseToken . Text.lines) (Text.readFile f)
-- FIXME its breaks sentence accuracy somehow...

eval flags [trainf,goldf,testf] = do
  (train,gold,test,baseline) <- fmap (\ (tr, g, t, b) -> (tr,toks g, toks t, fmap toks b)) (getEval flags trainf goldf testf)
  let seen = Set.fromList (map tokenForm train)
  dict  <- getDict flags . Just $ seen
  let isUnseen t = not (tokenForm t `Set.member` seen)
      isUnseenInDict t = not (lowercase (tokenForm t) `Map.member` dict)
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
      goldlemma g= map (fmap lowercase . tokenLemma) g
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
eval _flags _args = do
  error $ "GramLab.Morfette.Utils.eval: " 
        ++ "Incorrect arguments to command eval."
        
filterZip :: [Bool] -> [a] -> [a]
filterZip xs ys = catMaybes $ zipWith (\b x -> if b then Just x else Nothing) xs ys
    

getContents :: IO String
getContents = hSetBinaryMode stdin True >> UTF8.getContents
              
putStr :: String -> IO ()
putStr s = hSetBinaryMode stdout True >> UTF8.putStr s

putStrLn :: String -> IO ()
putStrLn s = hSetBinaryMode stdout True >> UTF8.putStrLn s
