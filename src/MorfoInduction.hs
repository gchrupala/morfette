import qualified GramLab.Data.Diff.EditTree as E
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function
import Data.Ord 
import Control.Monad.State
import Prelude hiding (sum)
import qualified System.Environment as Env
import qualified System.IO as IO
import Debug.Trace

size (E.Replace s z) = length s + length z
size (E.Split _ _ t u) = size t + size u

sum = List.foldl' (+) 0

main = do
  (command:args) <- Env.getArgs
  case command of
    "learn" -> do
         let [k] = args
         vocab <- (map trim . lines) `fmap` getContents
         let (xys,desc) = learn (read k) vocab
         IO.hPutStrLn  IO.stderr . show $ desc
         putStr . unlines . map format $ xys
    "score" -> do
         xys <- (map (\[w,s] -> (w,s)) . map words . lines ) `fmap` getContents
         let t = table . map fst $ xys
             desc = makeDesc t xys
         print desc

trim = unwords . words

format (form,stem) = unwords [form,stem]

type Table = Map.Map (String,String) (ETree,Int)

table xs = Map.fromList [ let t = E.make x x' 
                              r = (t,size t)
                          in (r == r `seq` (x,x'),r)
                          | x <- xs , x' <- xs ]

type Stem = String
type ETree = E.EditTree String Char

type Bag a = Map.Map a Int

empty :: Ord a => Bag a
empty = Map.empty

insert :: Ord a => a -> Bag a -> Bag a
insert x = Map.insertWith' (+) x 1 

remove :: Ord a => a -> Bag a -> Bag a
remove x b = case Map.findWithDefault 0 x b of
               0 -> b
               1 -> Map.delete x b
               n -> Map.adjust (\v -> v-1) x b

uniqSize :: Num n => Ord a => Bag a -> n
uniqSize = fromIntegral . Map.size

data Desc = Desc { stems :: Bag Stem 
                 , trees :: Bag ETree 
                 , descSize :: Int } 
            deriving (Show)

makeDesc :: Table -> [(String,Stem)] -> Desc
makeDesc t xys =
    let d =  List.foldl' (\d (form,stem) -> 
                          let (e,size_e) = t Map.! (stem,form)
                          in d { stems = insert stem . stems $ d
                               , trees = insert e . trees $ d }
                         ) 
                        (Desc empty empty 0)
                        xys
    in d { descSize =   computeDescSize (stems d) (trees d) }

computeDescSize stems trees = (sum . map length . Map.keys $ stems ) 
                              + (sum . map size . Map.keys $ trees )

updateDesc :: Table -> String -> Stem -> Stem -> Desc -> Desc
updateDesc t f s s' d =
    let (e,_) = t Map.! (s,f)
        (e',_) = t Map.! (s',f)
        stems' = insert s' . remove s . stems $ d
        trees' = insert e' . remove e . trees $ d 
    in Desc { stems = stems' , trees = trees' 
            , descSize = computeDescSize stems' trees' }

reassign :: [String] -> Table -> Desc -> String -> Stem ->  [((Stem,Desc),Int)]
reassign vocab t d form stem = 
   List.sortBy (comparing snd) 
   $ [ let d' = updateDesc t form stem v d 
       in  ((v,d'), descSize d')
           |      v <- vocab ]

iter :: Int -> [String] -> Table -> Desc -> [(String,Stem)] 
     -> [([(String,Stem)],Desc)]
iter k vocab t desc xys =  List.foldl' f [([],desc)] xys
    where f zsds (form,stem) = 
               take k . List.sortBy (comparing $ descSize . snd) $
                 [ ((form,stem'):zs,d') 
                 | (zs,d) <- zsds  
                 , let rs = take k . map fst . reassign vocab t d form $ stem 
                 , (stem',d') <- rs ]
                

learn :: Int -> [String] -> ([(String,Stem)],Desc)
learn k vocab =
    let t = table vocab
        xys = zip vocab $ reverse vocab
        desc = makeDesc t xys
        loop (zs,d) = 
            let (zs',d') = head $ iter k vocab t d zs
                size     = descSize d
                size'    = descSize d'
            in if --trace (show (size,size',size-size')) $
                  size' < size
               then loop (zs',d')
               else (zs,d)
    in loop (xys,desc)
