module GramLab.Morfette.Lexicon ( Lexicon
                                , Lex(..)
                                , makeLex
                                , emptyLex
                                , saveLex
                                , readLex
                                , toksToLexicon
                                , parseLexicon
                                )
where
import qualified Data.Map as Map
import qualified GramLab.Data.MultiSet as MS
import Data.Binary.Strict hiding (decode)
import qualified Data.Binary.Strict as Binary (decode)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes)
import GramLab.Utils (padRight,splitWith,splitInto,lowercase)
import GramLab.Morfette.Token

type Lexicon = Map.Map String [(String, String, Double)]
data Lex = Lex { dictLex  :: Lexicon
               , trainLex :: Lexicon }

instance Binary Lex where
    put (Lex x y) = put x >> put y
    get = do
      x <- get
      y <- get
      return (Lex x y)
emptyLex = Map.empty
makeLex = Lex
saveLex :: FilePath -> Lex -> IO ()
saveLex path lex = do
  BS.writeFile path (encode lex)

readLex :: FilePath -> IO Lex
readLex path = do
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
