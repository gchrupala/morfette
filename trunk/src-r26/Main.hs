
import GramLab.Utils (splitWith,join,lowercase)

import qualified POS   as P 
import qualified Lemma as L
import GramLab.Morfette.Models
import GramLab.Morfette.Utils (morfette)

main = morfette (prep,format) [P.featureSpec,L.featureSpec]


format toks = unlines . map (\ [Str f,Str p,ES s] -> unwords [f,L.apply s (lowercase f),p]) $ toks

prep (f,Just l, Just p) = [Str f,Str p, L.make f l]