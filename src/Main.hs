
import GramLab.Utils (splitWith,join,lowercase)

import qualified POS   as P 
import qualified Lemma as L
import GramLab.Morfette.Models
import GramLab.Morfette.Utils (morfette)
import Data.Binary.Strict
import qualified Data.ByteString.Lazy  as B
import  System.IO.UTF8
import Prelude hiding (putStrLn, getContents)
import Debug.Trace
main = morfette (prep,format) [P.featureSpec,L.featureSpec]
{-
main1 = do
  text <- getContents
  let xs = sentences text
      forms = map (map (take 1)) xs
      params = trainSettings
      beamSize = 3
  models' <- train [P.featureSpec,L.featureSpec] xs
  B.writeFile "model" (encode models')
  putStrLn "Models trained"
  ms <- fmap decode (B.readFile "model")
  let models = zipWith toModelFun [P.featureSpec,L.featureSpec] ms
      predicted = predict beamSize models forms
  putStrLn (join "\n" (map format predicted))

main2 = do
  text <- getContents
  let xs = sentences text
      forms = map (map (take 1)) xs
      params = trainSettings
      beamSize = 3
  models <- trainFun [P.featureSpec,L.featureSpec] xs
  putStrLn "Models trained"
  let predicted_pos = predict beamSize [models!!0] forms
  putStrLn (join "\n" (map show predicted_pos))
  let predicted_lem = predict beamSize [models!!1] predicted_pos
  putStrLn (join "\n" (map format predicted_lem))
  
sentences :: String -> [[Tok a]]
sentences text = map (map (\ [f,l,p] -> [f,p,l])) (splitWith null (map words (lines text)))
-}

format toks = unlines . map (\ [Str f,Str p,ES s] -> unwords [f,L.apply s (lowercase f),p]) $ toks

prep (f,Just l, Just p) = [Str f,Str p, L.make f l]