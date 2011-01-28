
import Aux.Utils (splitWith,join,lowercase)

import qualified POS   as P 
import qualified Lemma as L
import GramLab.Morfette.Models
import GramLab.Morfette.Utils (morfette)
import Debug.Trace

main = morfette (prep,format) [P.featureSpec,L.featureSpec]


format =   unlines
             . map (\ ([Str f,Str p, ES s]:ts) -> 
                    unwords $  [f, L.apply s . lowercase $ f, p ]
                            ++ [ unwords [L.apply s . lowercase $ f , p ]
                                     | [Str f, Str p , ES s ] <- ts ]
                   )

prep (f,Just l, Just p) = [Str f,Str p, L.make f l]
