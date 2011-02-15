module GramLab.Morfette.MWE ( detectMwes
                            , mweSet
                            , saveMwes
                            , loadMwes 
                            )
where
import GramLab.Utils (join,splitWith,lowercase)
import GramLab.Morfette.Token
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Char
import Data.Binary
import qualified Data.ByteString.Lazy as BS

mweSep = '_'

mweSet :: [Token] -> [[String]]
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

detectMwes :: [[String]] -> [String] -> [String]
detectMwes mwes = map (join [mweSep]) . compounds lowercase mwes

compounds :: (Ord a) => (a -> a) -> [[a]] -> [a] -> [[a]]
compounds f xxs []     = []
compounds f xxs (y:ys) = 
    let zs = map f (y:ys)
    in  case List.find (`List.isPrefixOf` zs) xxs of
          Nothing -> [y]:compounds f xxs ys
          Just x  -> let len = length x
                         (prefix,ys') = List.splitAt len (y:ys)
                     in prefix : compounds f xxs ys'

saveMwes path mwes = do
  BS.writeFile path (encode mwes)
  
loadMwes path = do
  s <- BS.readFile path
  return $ decode s

