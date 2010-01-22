{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Char
data Tok     = Tok { form :: String , lemma :: String , pos :: String } deriving (Show,Read)
data Context = Context
tokenAt :: Int -> Context -> Tok
tokenAt = undefined
trainPosMap :: String -> Context -> [String]
trainPosMap = undefined

data Index   = Index Int deriving (Show,Read)
data Field a = Form a | Lemma a | POS a deriving (Show,Read)

data StringFun1 a = Prefix Int a 
                   | Suffix Int a 
                   | Reverse a
                   | Lowercase a
                     deriving (Show,Read)
data StringFun2 a b = Cat a b
data Sets a = TrainPosSet a
            | DictPosSet a
            | TrainScriptSet a
            | DictScriptSet a
data SetUnion a b = Union a b

--data Map f a = Map f [a] deriving (Show,Read)

class Eval a b | a -> b where 
    eval :: a -> Context -> b
instance                 Eval Index       Tok      where 
    eval (Index i)    = tokenAt i
instance (Eval a Tok) => Eval (Field  a)  String   where
    eval (Form t)     = form  . eval t
    eval (Lemma t)    = lemma . eval t
    eval (POS  t)     = pos   . eval t
instance (Eval a String) => Eval (StringFun1 a) String where
    eval (Suffix i t) = (\xs -> let x = eval t xs in reverse (take i (reverse x)))
    eval (Prefix i t) = take i . eval t
    eval (Reverse t)  = reverse . eval t
    eval (Lowercase t) = map toLower . eval t
instance (Eval a String,Eval b String) => Eval (StringFun2 a b) String where
    eval (Cat a b) c = eval a c ++ eval b c
instance (Eval a String) => Eval (Sets a) [String] where
    eval (TrainPosSet a) c = trainPosMap (eval a c) c
    -- etc
instance (Eval a [t],Eval b [t]) => Eval (SetUnion a b) [t] where
    eval (Union a b) c = eval a c ++ eval b c

--instance (Eval a ra, Eval f (ra -> b)) => Eval (Map f a) [b] where
--    eval (Map f as) xs = map (eval f xs) (map (\a -> eval a xs) as)

              
{-
class Eval a where
    type Result a  
    eval :: a -> ([Tok] -> Result a)
instance Eval Index where 
    type Result Index = Tok 
    eval (Index i) = (!!i)
instance (Eval a,Result a ~ Tok) => Eval (Field a) where
--instance  Eval (Field Index) where
    type Result (Field a) = String
    eval (Form a)  = form  . eval a 
    eval (Lemma a) = lemma . eval a
    eval (POS a)   = pos   . eval a
-}
{-
data Tok = Tok { form :: String , lemma :: String , pos :: String }
data Fun a where
    Index  :: Int -> Fun Tok
    Form   :: Fun Tok -> Fun String
    Lemma  :: Fun Tok -> Fun String
    POS    :: Fun Tok -> Fun String
    Suffix :: Int -> Fun String -> Fun String 
    Prefix :: Int -> Fun String -> Fun String
instance Show (Fun a) where
    show (Index i) = "Index " ++ show i
    show (Form s)  = "Form " ++ "(" ++ show s ++ ")"
    show (Lemma s) = "Lemma " ++ "(" ++ show s ++ ")"
    show (POS s)   = "POS " ++ "(" ++ show s ++ ")"
    show (Suffix i s) = "Suffix "  ++ show i ++ " " ++ "(" ++ show s ++ ")"
    show (Prefix i s) = "Suffix "  ++ show i ++ " " ++ "(" ++ show s ++ ")"

eval :: Fun a -> [Tok] -> a
eval (Index i) = (!!i)
eval (Form t)  =  form  . eval t
eval (Lemma t) =  lemma . eval t
eval (POS t)   =  pos  . eval t
eval (Suffix i t) = (\xs -> let x = eval t xs in reverse (take i (reverse x)))
eval (Prefix i t) = take i . eval t
-}