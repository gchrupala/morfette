module GramLab.Morfette.MWE ( detectMwes
                            , mweSet
                            , saveMwes
                            , loadMwes 
                            )
where
import Aux.Utils (splitWith)
import GramLab.Morfette.Token
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Char
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Aux.Text  as Text
type Txt = Text.Txt

mweSep = '_'

mweSet :: [Token] -> [[Txt]]
mweSet toks = List.sortBy (flip (comparing length))
              . map fst
              . filter ((> 1) . snd)
              . Map.toList
              . Map.fromListWith (+)
              . map (\x -> (x,1)) 
              . map (splitWith (==mweSep))
              . filter common
              . map tokenForm $ toks
    where common = all (\c -> isLower c || c `elem` [mweSep,'-'])

detectMwes :: [[Txt]] -> [Txt] -> [Txt]
detectMwes mwes [] = []
detectMwes mwes (x:xs) = case List.find (`List.isPrefixOf` xs') mwes of
                           Nothing -> x:detectMwes mwes xs
                           Just mwe -> let len = Text.length mwe 
                                       in join [mweSep] (take len (x:xs)) : drop len (x:xs)
    where  xs' = map lowercase (x:xs)                         

saveMwes path mwes = do
  BS.writeFile path (encode mwes)
  
loadMwes path = do
  s <- BS.readFile path
  return $ decode s

join sep = List.concat . List.intersperse sep 
lowercase = Text.toLower