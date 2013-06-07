
import GramLab.Utils (splitWith,join,lowercase)

import qualified POS   as P 
import qualified Lemma as L
import GramLab.Morfette.Models
import GramLab.Morfette.Utils (morfette)
import GramLab.Morfette.Token
import Debug.Trace

main = morfette (prep,format) [P.featureSpec,L.featureSpec]


format =   unlines
             . map (\ ([Str f,Str p, ES s]:ts) -> 
                    unwords $  [f, L.apply s . lowercase $ f, p ]
                            ++ [ unwords [L.apply s . lowercase $ f , p ]
                                     | [Str f, Str p , ES s ] <- ts ]
                   )

prep t  = [ Str (tokenForm t)
          , Str (maybe (error "Main: missing POS") id (tokenPOS t))
          , L.make (tokenForm t) (maybe (error "Main: missing lemma") id (tokenLemma t))
          , Embed (tokenEmbedding t)
          ]
