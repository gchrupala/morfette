module GramLab.Morfette.Lang.Conf ( Lexicon
                                  , Conf(..)
                                  , Lang 
                                  , makeConf
                                  , emptyLexicon
                                  , saveConf
                                  , readConf
                                  , parseLexicon
                                  , splitPOS
                                )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Binary hiding (decode)
import qualified Data.Binary as Binary (decode)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes)
import GramLab.Utils (padRight,splitWith,splitInto,lowercase)
import GramLab.Morfette.Token
import Debug.Trace
import qualified Control.Monad.State as S
    
type Lang = String
type Lexicon = Map.Map String [(String, String)]
data Conf = Conf { dictLex  :: Lexicon
                 , lang     :: Lang } deriving (Eq)

instance Binary Conf where
    put (Conf x y) = put x >> put y
    get = do
      x <- get
      y <- get
      return (Conf x y)

emptyLexicon = Map.empty

makeConf = Conf

saveConf :: FilePath -> Conf -> IO ()

saveConf path lex = do
  BS.writeFile path (encode lex)

readConf :: FilePath -> IO Conf
readConf path = do
  txt <- BS.readFile path
  return (Binary.decode txt)

parseLexicon :: Maybe (Set.Set String) -> String -> Lexicon 
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
           . lines 

parseEntry :: String -> [(String,(String, String))]
parseEntry line = 
    let (form:pairs) = words line 
    in [ lemma == lemma && pos == pos `seq` (form,(lemma,lowercase pos))
             | [lemma,pos] <- splitInto 2 $ pairs ]



splitPOS :: Lang -> String -> [String]
splitPOS "tr" = splitWith (=='+')
splitPOS "pl" = splitWith (==':')
splitPOS "cy" = splitWith (=='-')
splitPOS "ga" = splitWith (=='-')
splitPOS _    = map return

atomize str = do
  d <- S.get
  case Map.lookup str d of
    Nothing -> do S.put (Map.insert str str d)
                  return str
    Just str' -> return str'
