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

parseLexicon :: Set.Set String -> String -> Lexicon 
parseLexicon toks = Map.fromList 
                         . filter (\(w,_) -> w `Set.member` toks)
                         . map parseEntry 
                         . lines 
                 
    
parseEntry :: String -> (String, [(String, String)])
parseEntry line = 
    let (form:pairs) = words line 
        len = fromIntegral $ length pairs
        val = map (\ [lemma,pos] -> (lowercase lemma,lowercase pos)) 
                       . splitInto 2 
                       $ pairs
    in  (val == val `seq` form,val)


splitPOS :: Lang -> String -> [String]
splitPOS "tr" = splitWith (=='+')
splitPOS "pl" = splitWith (==':')
splitPOS "cy" = splitWith (=='-')
splitPOS "ga" = splitWith (=='-')
splitPOS _    = map return
