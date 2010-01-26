{-# LANGUAGE NoMonomorphismRestriction#-} 

module GramLab.Intern ( Table(..)
                      , initial
                      , intern
                      , internMany
                      , unintern
                      , uninternMany
                      , maybeIntern
                      , maybeInternMany
                      , module Control.Monad.State
                      )
where
import Prelude hiding (mapM)
import qualified Data.Map as Map
import Control.Monad.State hiding (mapM)
import Data.Traversable (mapM)
import qualified Data.Binary as B

data Table a = T !Int (Map.Map a Int) deriving (Eq,Read,Show)



initial = (T 0 Map.empty)

intern :: (Ord k) => k -> State (Table k) Int
intern s = do
  (T i m) <- get
  case Map.lookup s m of
    Nothing -> do 
             put (T (i+1) (Map.insert s i m))
             return i
    Just j  -> return  j

internWith hget s = do
  (T i m) <- get
  case Map.lookup s m of
    Nothing -> do 
      put (T (i+1) (Map.insert s i m))
      return i
    Just j  -> return  j



internMany = mapM intern


-- only read existing entries in state
maybeIntern s = do
  (T i m) <- get
  return $ Map.lookup s m 

maybeInternMany = mapM maybeIntern


unintern i = do
  m <- get
  return $ case Map.lookup i m of { Just x -> x }

uninternMany = mapM unintern

instance (Ord a, B.Binary a) => B.Binary (Table a) where
    put (T i m) = B.put i >> B.put m
    get = liftM2 T B.get B.get
