module GramLab.Morfette.ES.Lexicon (lexicon)
where 
import qualified Data.Map as Map
import System.IO.Unsafe
import qualified System.IO.UTF8 as UIO
import GramLab.Utils (lowercase,splitInto)
import Paths_morfette


lexicon = unsafePerformIO $  do 
            file <- getDataFileName "dicc.src"
            txt  <- UIO.readFile file
            return (readFreelingLexicon txt)

readFreelingLexicon text = Map.fromList . map parseEntry . lines $ text
    
parseEntry :: String -> (String, [(String, String)])
parseEntry line = 
    let (form:pairs) = words line 
    in  (form,(map (\ [lemma,pos] -> (lowercase lemma,lowercase pos)) (splitInto 2 pairs)))
