module GramLab.Morfette.Lang.Conf ( Lexicon
                                  , ClusterDict
                                  , Conf(..)
                                  , Lang 
                                  , makeConf
                                  , emptyLexicon
                                  , saveConf
                                  , readConf
                                  , parseLexicon
                                  , parseClusterDict
                                  , splitPOS
                                )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Binary hiding (decode)
import qualified Data.Binary as Binary (decode)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes)
import GramLab.Morfette.Token
import Debug.Trace
import qualified Control.Monad.State as S
import qualified Aux.Text as Text 
type Txt = Text.Txt
    
type Lang = String
type Lexicon = Map.Map Txt [(Txt, Txt)]
type ClusterDict = Map.Map Txt Txt

data Conf = Conf { dictLex  :: Lexicon
                 , clusterDict :: ClusterDict 
                 , lang     :: Lang } deriving (Eq)

instance Binary Conf where
    put (Conf x y z) = put x >> put y >> put z
    get = do
      x <- get
      y <- get
      z <- get
      return (Conf x y z)

emptyLexicon = Map.empty

makeConf = Conf

saveConf :: FilePath -> Conf -> IO ()

saveConf path lex = do
  BS.writeFile path (encode lex)

readConf :: FilePath -> IO Conf
readConf path = do
  txt <- BS.readFile path
  return (Binary.decode txt)

parseClusterDict :: Txt -> ClusterDict
parseClusterDict =   Map.fromList 
                 . map (\ln -> let [w,c] = Text.words ln in (w,c)) 
                 . Text.lines             

parseLexicon :: Maybe (Set.Set Txt) -> Txt -> Lexicon 
parseLexicon toks = 
    Map.fromListWith (++)
           . flip S.evalState Map.empty 
           . mapM (\(f,(l,p)) -> do
                     f' <- atomize f
                     l' <- atomize l
                     p' <- atomize p
                     return (f',[(l',p')]))
           . maybe id (\d -> filter (\(w,_) -> w `Set.member`d)) toks
           . concatMap parseEntry 
           . Text.lines 

parseEntry :: Txt -> [(Txt,(Txt, Txt))]
parseEntry line = 
    let (form:pairs) = Text.words line 
    in [ lemma == lemma && pos == pos `seq` (form,(lemma,Text.lowercase pos))
             | [lemma,pos] <- splitInto 2 $ pairs ]



splitPOS :: Lang -> Txt -> [Txt]
splitPOS "tr" = Text.splitWith (=='+')
splitPOS "pl" = Text.splitWith (==':')
splitPOS "cy" = Text.splitWith (=='-')
splitPOS "ga" = Text.splitWith (=='-')
splitPOS _    = map return

atomize str = do
  d <- S.get
  case Map.lookup str d of
    Nothing -> do S.put (Map.insert str str d)
                  return str
    Just str' -> return str'

splitInto :: Int -> [a] -> [[a]]
splitInto size [] = []
splitInto size xs = let (ws,ys) = splitAt size xs
                    in  ws:splitInto size ys