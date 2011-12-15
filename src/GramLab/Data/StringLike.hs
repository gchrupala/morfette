{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies 
, FlexibleInstances #-}
module GramLab.Data.StringLike (StringLike(..))
where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as B
import qualified Data.List as List
import Data.Word
import Prelude hiding (length,null,tail,init,splitAt,reverse,map,foldl,foldr)
import qualified Prelude as P
import Codec.Binary.UTF8.String
class StringLike seq a | seq -> a where
    toString   :: seq -> [a]
    fromString :: [a] -> seq
    length     :: seq -> Int
    length     =  P.length . toString
    null       :: seq -> Bool
    null       =  P.null .  toString
    tail       :: seq -> seq 
    tail       =  fromString . P.tail . toString
    init       :: seq -> seq
    init       =  fromString . P.init . toString
    tails      :: seq -> [seq]
    tails      =  P.map fromString . List.tails . toString
    inits      :: seq -> [seq]
    inits      =  P.map fromString . List.inits . toString
    splitAt    :: Int -> seq -> (seq,seq)
    splitAt i w = let (w',w'') = P.splitAt i (toString w) in (fromString w', fromString w'')
    reverse    :: seq -> seq
    reverse    =  fromString . P.reverse . toString
    append     :: seq -> seq -> seq
    append w w' = fromString (toString w ++ toString w')
    cons       :: a -> seq -> seq
    cons x xs  = fromString $ x: toString xs
    uncons     :: seq -> Maybe (a,seq)
    uncons xs  = case toString xs of
                   (x:xs) -> Just (x,fromString xs)
                   _      -> Nothing
    map        :: (a -> a) -> seq -> seq
    map f      = fromString . map f . toString

instance StringLike [a] a where
    toString = id
    fromString = id

instance StringLike B.ByteString Char where
    toString   = decode . B.unpack
    fromString = B.pack . encode
    null    = B.null
    append  = B.append


instance StringLike L.ByteString Char where
    toString   = decode . L.unpack
    fromString = L.pack . encode
    null    = L.null
    append  = L.append
