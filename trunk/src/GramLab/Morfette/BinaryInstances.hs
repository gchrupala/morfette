module GramLab.Morfette.BinaryInstances 
where
import Data.Binary
import Control.Monad (liftM)
import qualified Data.Vector.Unboxed as U

instance (U.Unbox a, Binary a) => Binary (U.Vector a) where
  put v = put (U.toList v)
  get = liftM U.fromList get