{-# OPTIONS_GHC -fglasgow-exts #-}
import qualified System.IO.UTF8 as UIO
import System.IO (stderr)
import Prelude hiding (readFile, writeFile, getContents)
import GramLab.Morfette.Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GramLab.Data.MultiSet as MSet
import Data.Maybe
import System(getArgs)
import GramLab.Utils
import System
import GramLab.Commands
import System.Console.GetOpt
import Text.Printf
import GramLab.Morfette.LZipper 
import qualified  GramLab.Morfette.LZipper as LZ (fromList)
import Debug.Trace
import qualified Prelude
import Control.Exception
import Control.Arrow (second)
import Control.Monad (when)
import Data.List (sortBy,transpose)
import Data.Ord
import qualified GramLab.Maxent.ZhangLe.Model    as M
--import qualified GramLab.Morfette.Features.Lemma1   as LF1
import qualified GramLab.Morfette.Features.Lemma8   as LF
--import qualified GramLab.Morfette.Features.Lemma8simple   as LF
--import qualified GramLab.Morfette.Features.Lemma8flip   as LF
import GramLab.Morfette.Features.LemmaUtil
import qualified GramLab.Morfette.Features.Tagging3 as TF
--import qualified GramLab.Morfette.Features.Tagging3simple as TF

--import qualified GramLab.Morfette.Features.Tagging3flip as TF
--import qualified GramLab.Morfette.Features.Tagging3joint as TF
--import qualified GramLab.Morfette.ES.Tagging3 as TF
import GramLab.Morfette.Features.Common
import GramLab.Morfette.Config6


main = defaultMain commands "Usage: morfette command [OPTION...] [ARG...]"

commands = [
             ("train" , CommandSpec train
                          "train maxent model"
                          [ Option [] ["model-prefix"] (ReqArg ModelPrefix "STRING")
                                       "path prefix to model files"
                          , Option [] ["config-file"] (ReqArg ConfigFile "PATH")
                                       "path to config file"
                          , Option [] ["mode"]        (ReqArg Mode "STRING")
                                       "lemma|postag|combined"
                          ] )
           , ("predict" , CommandSpec predict
                            "predict lemmas using saved model data"
                            [ Option [] ["config-file"] (ReqArg ConfigFile "PATH")
                                         "path to config file"
                            , Option [] ["output"] (ReqArg OutputPath "PATH")
                                         "path to output file" 
                            , Option [] ["beam"]   (ReqArg (BeamSize . read) "+INT")
                                         "beam size to use"
                            , Option [] ["preprune","preprune-threshold"]  (ReqArg (Preprune . read) "[0..1]")
                                         "prepruning threshold" 
                            , Option [] ["eval"]   (NoArg Eval)
                                         "evaluate lemmatization accuracy"
                            , Option [] ["pos-prefix-for-eval","pos-eval"] (ReqArg (POSPrefixEval . read) "+INT")
                                         "POS prefix to consider in evaluation"
                            , Option [] ["mode"]        (ReqArg Mode "STRING")
                                         "lemma|postag"
                            ] )
           , ("eval" , CommandSpec eval 
                          "evaluate morpho-tagging and lemmatization results"
                          [])
           ]
            
data Flag = OutputPath FilePath
          | ModelPrefix String
          | POSPrefixEval Int
          | Eval
          | BeamSize Int 
          | Preprune Double
          | ConfigFile FilePath
          | Mode String
          deriving Eq

         
modelFile prefix = prefix ++ ".model"
lemmaModelFile prefix = prefix ++ ".lemma-model"
posModelFile    prefix = prefix ++ ".pos-model"
paramFile prefix = prefix ++ ".param"

getParams flags = do 
  params <- case [f | ConfigFile f <- flags] of 
              []  -> return defaults
              [f] -> fmap Prelude.read (UIO.readFile f)
  return params

configFromParams params flags =
  case [f | Mode f <- flags ] of
    ["postag"] -> getPOSParams   params
    ["lemma"]  -> getLemmaParams params
    ["joint"]  -> getPOSParams   params

getLemmaParams params = (LF.makeFeatureSpec (fst . lemmaConfig    $ params),snd . lemmaConfig     $ params)
getPOSParams   params = (TF.makeFeatureSpec (fst . taggingConfig  $ params),snd . taggingConfig   $ params)

train :: Command Flag
train flags args@(path:_) = do
  case [f | Mode f <- flags ] of
    ["combined"] -> train2 flags args
    [mode]            -> do 
      let f | mode == "lemma"  = justSES
            | mode == "postag" = justPOS
            | mode == "joint"  = justBoth
      params <- getParams flags
      let (featSpec, trainParams) = configFromParams params flags
      toks <- fmap (map parseToken . lines) (UIO.readFile path)
      let sentences = splitWith isNullToken toks
          modelprefix = case [f | ModelPrefix f <- flags ] of { [f] -> f ; _ -> path }
          examples = (concatMap (map (exampleToPair f) . extractFeatures featSpec) $ sentences)
      UIO.writeFile (paramFile modelprefix) (show params)
      model <- M.train trainParams examples
      M.save (modelFile modelprefix) model                    
                                                          

train2 flags (path:_) = do
  params <- getParams flags
  let (lemmaFeatSpec,lemmaTrainParams) = getLemmaParams params
      (posFeatSpec,  posTrainParams)   = getPOSParams params
  toks <- fmap (map parseToken . lines) (UIO.readFile path)
  let sentences = splitWith isNullToken toks
      modelprefix = case [f | ModelPrefix f <- flags ] of { [f] -> f ; _ -> path }
      lemmaExamples = makeExamples justSES lemmaFeatSpec sentences
      posExamples   = makeExamples justPOS posFeatSpec   sentences
  UIO.writeFile (paramFile modelprefix) (show params)
  UIO.hPutStrLn stderr "Training POS-tagger"
  posModel   <- M.train posTrainParams   posExamples
  M.save (posModelFile modelprefix) posModel
  UIO.hPutStrLn stderr "Training lemmatizer"
  lemmaModel <- M.train lemmaTrainParams lemmaExamples
  M.save (lemmaModelFile modelprefix) lemmaModel


makeExamples f spec = concatMap (map (exampleToPair f) . extractFeatures spec) 

extractFrequent i  =   MSet.foldOccur (\x j z -> if j >= i then Set.insert x z else z) Set.empty 
                     . MSet.fromList 
                     . map lowercase

predict :: Command Flag
predict flags args@(modelprefix:_) = do
  case [f | Mode f <- flags ] of
    ["combined"] -> predict2 flags args
    [mode]       -> do
      let f | mode == "lemma"  = justSES
            | mode == "postag" = justPOS
            | mode == "joint"  = justBoth
      toks <- fmap (map parseToken . lines) UIO.getContents
      let sentences = splitWith isNullToken toks
          defaultBeamSize = 3
      params <- fmap Prelude.read $ UIO.readFile (paramFile modelprefix)
      let (featSpec,trainParams) = configFromParams params flags
          n = case [f | BeamSize f <- flags ] of { [f] -> f ; _ -> defaultBeamSize }
      model <- M.load (modelFile modelprefix)
      let { classifier = mkClassifier f model featSpec
          ; sws = map (LZ.fromList . map (tokenToExample featSpec)) sentences
          ; nbests = map (classifyBeam n classifier (*) . (\z -> [(1,z)])) sws -- cool: (\z -> [(1,z)]) == return . (,) 1
          ; doJoint = do {
             let { predicted_pos    = map (map getPOS . bestClassSeq . unzip) nbests
                 ; predicted_lemmas = (flip . flip zipWith) nbests sentences 
                                      $ (\z toks -> 
                                            zipWith applyScript 
                                                        (map getSES . bestClassSeq . unzip $ z) 
                                                        (map tokenForm toks)) 
                 ; predicted_sents  = map (\(forms,lemmas,postags) -> zip3 forms lemmas postags)
                                           (zip3 (map (map tokenForm) sentences)
                                                 predicted_lemmas
                                                 predicted_pos)
                 
{-               ; gold_pos         = map (map (fromMaybe (assert False undefined) . tokenPOS)) sentences
                 ; gold_lemmas      = map (map (fromMaybe (assert False undefined) . tokenLemma)) sentences
                 ; total_pos     = sum (map length gold_pos)
                 ; correct_pos   = length . filter (uncurry (==)) $ zip (concat predicted_pos) (concat gold_pos)
                 ; total_lemmas   = sum (map length gold_lemmas)
                 ; correct_lemmas = length . filter (uncurry (==)) $ zip (concat predicted_lemmas) (concat gold_lemmas) 
-}
                 } 
             in do mapM_ (UIO.putStrLn . unlines . map formatTriple) predicted_sents }
--             in do {when (Eval `elem` flags) (do { UIO.hPutStrLn stderr $ showAcc (correct_pos,total_pos);
--                                        UIO.hPutStrLn stderr $ showAcc (correct_lemmas, total_lemmas) })}
                       }
      case [f | Mode f <- flags ] of 
        ["joint"] -> doJoint
        f         -> do {
                      let { (predicted,gold) = 
                                 case f of
                                   ["lemma"] -> let predicted = (flip . flip zipWith) nbests sentences 
                                                                $ (\z toks -> 
                                                                       zipWith applyScript 
                                                                                   (map getSES . bestClassSeq . unzip $ z) 
                                                                                   (map tokenForm toks))
                                                    gold      = map (map (lowercase . fromMaybe (assert False undefined) . tokenLemma)) sentences
                                                in (predicted,gold)
                                   ["postag"] -> let predicted = map (map getPOS . bestClassSeq . unzip) nbests
                                                     gold      = map (map (fromMaybe (assert False undefined) . tokenPOS)) sentences
                                                 in (predicted,gold)
                           ; total     = sum (map length gold)
                           ; correct   = length . filter (uncurry (==)) $ zip (concat predicted) (concat gold) }
                      in do {   mapM_ (UIO.putStrLn . unlines) predicted 
                              ; when (Eval `elem` flags) (UIO.hPutStrLn stderr $ showAcc (correct,total)) } }

predict2 flags (modelprefix:_) = do
   toks <- fmap (map parseToken . lines) UIO.getContents
   let sentences = splitWith isNullToken toks
       defaultBeamSize = 3
       defaultPpruneTh = 0.3
   params <- fmap Prelude.read $ UIO.readFile (paramFile modelprefix)
   let (lemmaFeatSpec,lemmaTrainParams) = getLemmaParams params
       (posFeatSpec,  posTrainParams)   = getPOSParams params
       n = case [f | BeamSize f <- flags ] of { [f] -> f ; _ -> defaultBeamSize }
       c = case [f | Preprune f <- flags ] of { [f] -> f ; _ -> defaultPpruneTh }
   lemmaModel <- M.load (lemmaModelFile modelprefix) -- has to be possible to load from different files
   posModel   <- M.load (posModelFile modelprefix)
   let lemmatizer = mkClassifier justSES lemmaModel lemmaFeatSpec
       postagger  = mkClassifier justPOS posModel   posFeatSpec
       zs = map (\s -> ( LZ.fromList . map (tokenToExample lemmaFeatSpec) $ s
                       , LZ.fromList . map (tokenToExample posFeatSpec)   $ s)) sentences
       nbests = map (classifyBeam3 n c lemmatizer postagger . (\zp -> [(1,zp)])) zs
       sentences'  = zipWith bestTokSeq sentences nbests
   mapM_ (UIO.putStrLn . unlines . map formatToken) sentences'
   when (Eval `elem` flags) $ UIO.hPutStrLn stderr $ showAccuracy $ tokAccuracy' (concat sentences) (concat sentences')



eval _ (paramf:trainf:goldf:testf:_) = do
  params <- fmap Prelude.read $ UIO.readFile paramf
  train <- getToks trainf
  gold  <- getToks goldf
  test  <- getToks testf
  let (lemmaFeatSpec,lemmaTrainParams) = getLemmaParams params
      seen = Set.fromList (map tokenForm train)
      isUnseen (form,_,_) = not (form `Set.member` seen)
      all_acc    = tokAccuracy' gold test
      unseen_acc = tokAccuracy' (filter isUnseen gold) (filter isUnseen test)
      goldses  g  = map (fromSES . justSES . tokenClass lemmaFeatSpec) g
      uniqueSES  = fromIntegral $ Set.size $ Set.fromList $ goldses gold
      uniquePOS  = fromIntegral $ Set.size $ Set.fromList $ map tokenPOS gold
      bs_lemma_acc gs = (fromIntegral  (length (filter isNullLemmaClass gs))
                                           / fromIntegral (length gs))
  UIO.putStrLn $ "Accuracy all:\n" ++ showAccuracy all_acc
  UIO.putStrLn $ "Accuracy unseen:\n" ++ showAccuracy unseen_acc
  UIO.putStrLn $ "Baseline lemma all acc: " ++ show (bs_lemma_acc (goldses gold))
  UIO.putStrLn $ "Baseline lemma unseen acc: " ++ show (bs_lemma_acc (goldses (filter isUnseen gold)))
  UIO.putStrLn $ "Unique lemma-class: " ++ show uniqueSES
  UIO.putStrLn $ "Unique  morpho-tag: " ++ show uniquePOS
  where getToks f = fmap (filter (not . isNullToken) . map parseToken . lines) (UIO.readFile f)
    

-- nbest::[(Double,[(Label,Label)])]         -- nbest list of pairs of labels
bestTokSeq toks nbest = zipWith (\(f,ml,mp) (ses,pos) -> (f
                                                         , Just $ applyScript (getSES ses) f
                                                         , Just (getPOS pos))) 
                                 toks 
                                 best
    where best = let x:_ = nbest in snd x 
formatToken (f,ml,mp) = unwords [f,fromMaybe "" ml,fromMaybe "" mp]
formatTriple (f,ml,mp) = unwords [f,ml,mp]
bestClassSeq (_,xs:xss) = xs         


seqProb x = product . map fst . left $ x

showPair (form,lemma) = unwords [form, lemma]

data Accuracy = Acc { lemmaAcc :: (Int,Int)
                    , posAcc   :: (Int,Int) }

tokAccuracy' xs ys = tokAccuracy (map tokToPair xs) (map tokToPair ys)
    where tokToPair (form,lemma,pos) = (fromMaybe form lemma,pos)
tokAccuracy xs ys = Acc { lemmaAcc = (correct $ lowercase . fst,total)
                        , posAcc   = (correct snd,total) }
    where total = length xs
          correct f = length $ filter id $ zipWith (\x y -> f x == f y) xs ys


showAccuracy acc = unlines [ "Lemma acc: " ++ (showAcc . lemmaAcc) acc
                           , "POS   acc: " ++ (showAcc . posAcc  ) acc ]
showAcc (correct,total) = printf "%.2f%% (%d / %d)" 
                                      (100* (fromIntegral correct::Double)/(fromIntegral total::Double))
                                      correct 
                                      total
