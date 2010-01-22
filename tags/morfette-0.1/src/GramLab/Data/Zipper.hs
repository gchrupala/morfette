module GramLab.Data.Zipper ( Cursor(..)
                           , Path(..)
                           , fromTree
                           , moveRight
                           , moveLeft
                           , moveUp
                           , moveDown
                           , toList
                            )
where
import Data.Tree
import Data.List (unfoldr)
import Control.Monad 

data Path a     = Top
                | Point  { left    :: [Tree a]
                         , here    :: a
                         , right   :: [Tree a]
                         , up      ::  Path a  } deriving (Show,Eq)

data Cursor a   = Cursor { this    :: Tree a
                         , context :: Path a   } deriving (Show,Eq)
fromTree t = Cursor { this = t , context = Top }

errmsg dir = "GramLab.Data.Zipper.move"++dir++": Cannot move"

moveRight :: (Monad m) => Cursor a -> m (Cursor a)
moveRight c@(Cursor t p) = do
    case p of
      Top                    -> fail (errmsg "Right")
      Point { right = [] }   -> fail (errmsg "Right")
      Point ls here (r:rs) p -> return $ Cursor { this = r
                                                , context = Point (t:ls) here rs p }

moveLeft  :: (Monad m) => Cursor a -> m (Cursor a)
moveLeft  c@(Cursor t p) = do
    case p of
      Top                    -> fail (errmsg "Left")
      Point { left = [] }    -> fail (errmsg "Left")
      Point (l:ls) here rs p -> return $ Cursor { this = l
                                                , context = Point ls here (t:rs) p }

moveUp    :: (Monad m) => Cursor a -> m (Cursor a)
moveUp c@(Cursor t p) =  do
    case p of
      Top                 -> fail (errmsg "Up")
      Point ls here rs up -> return $ Cursor { this = Node here (reverse ls ++ t : rs)
                                             , context = up }

moveDown  :: (Monad m) => Cursor a -> m (Cursor a)
moveDown c@(Cursor (Node l ns) p) = do
    case ns of
      []   -> fail (errmsg "Down")
      n:ns -> return $ Cursor { this = n , context = Point [] l  ns p } 

keep f c = 
  case f c of
    Nothing -> c
    Just c  -> keep f c
next c = 
    case moveRight c of
      Just c' -> return $ keep moveDown c'
      Nothing -> moveUp c

toList c = let c' = keep moveDown c
           in c':unfoldr (fmap (join (,)) . next) c'

