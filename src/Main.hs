
import GramLab.Utils (splitWith,join,lowercase)

import qualified POS   as P 
import qualified Lemma as L
import GramLab.Morfette.Models2
import GramLab.Morfette.Utils 
import GramLab.Morfette.Token
import Debug.Trace

main = morfette (prep, format) [P.featureSpec, L.featureSpec]


{-
format =   unlines
             . map (\ ([Str f,Str p, ES s]:ts) -> 
                    unwords $  [f, L.apply s . lowercase $ f, p ]
                            ++ [ unwords [L.apply s . lowercase $ f , p ]
                                     | [Str f, Str p , ES s ] <- ts ]
                   )
-}
format :: [[ROW]] -> String
format rss =   
  let one (Row { input = Input { inputForm = f }
               , output = [ POS pos, ET et ] }) = unwords [f, L.apply et . lowercase $ f, pos ] 
  in  unlines [ unwords [ one r | r <- rs ] | rs <- rss ]

prep :: Token -> ROW
prep t  = 
  Row { input  = Input { inputForm = tokenForm t
                       , inputEmb  = tokenEmb  t }
      , output = [ POS (maybe (error "Main: missing POS") id (tokenPOS t)) 
                 , L.make (tokenForm t) (maybe (error "Main: missing lemma") id (tokenLemma t)) 
                 ]
      }
  
