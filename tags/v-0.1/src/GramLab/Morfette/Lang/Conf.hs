module GramLab.Morfette.Lang.Conf ( Lexicon
                                  , Conf(..)
                                  , Lang 
                                  , makeConf
                                  , emptyLexicon
                                  , saveConf
                                  , readConf
                                  , toksToLexicon
                                  , parseLexicon
                                  , splitPOS
                                )
where
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import Data.Binary.Strict hiding (decode)
import qualified Data.Binary.Strict as Binary (decode)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes)
import GramLab.Utils (padRight,splitWith,splitInto,lowercase)
import GramLab.Morfette.Token

type Lang = String
type Lexicon = Map.Map String [(String, String, Double)]
data Conf = Conf { dictLex  :: Lexicon
                 , trainLex :: Lexicon 
                 , lang     :: Lang }

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

toksToLexicon :: [Token] -> Lexicon
toksToLexicon xs = Map.map (\ys -> let ms = MS.fromList ys
                                       size = fromIntegral $ MS.size ms
                                   in  map (\((lemma,pos),count) 
                                                    -> (lemma,pos,fromIntegral count/size)) (MS.toOccurList ms))
                       . foldr (\(form,lemma,pos) m -> Map.insertWith (++) form [(lemma,pos)] m) Map.empty 
                       . catMaybes $ flip map xs $ \x ->
                           case x of
                             (form, Just lemma, Just pos) -> Just (lowercase form, lowercase lemma,lowercase pos)
                             _                            -> Nothing

parseLexicon text = Map.fromList . map parseEntry . lines $ text
    
parseEntry :: String -> (String, [(String, String, Double)])
parseEntry line = 
    let (form:pairs) = words line 
        len          = fromIntegral $ length pairs
    in  (form,(map (\ [lemma,pos] -> (lowercase lemma,lowercase pos,1/len)) (splitInto 2 pairs)))


splitPOS :: Lang -> String -> [String]
splitPOS "tr" = splitWith (=='+')
splitPOS "pl" = splitWith (==':')
splitPOS "cy" = splitWith (=='-')
splitPOS "ga" = splitWith (=='-')
splitPOS _    = map return
