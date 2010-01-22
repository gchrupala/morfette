module GramLab.SexpParser( Sexp(..)
                         , parseExpr
                         , parseExprRest
                         , fromString
                         , prettyPrint
                         , removeComments
)
where
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad hiding (join)
import GramLab.Utils (join)
import Text.PrettyPrint (text,hsep,render,parens)
import Debug.Trace
leftP  = '('
rightP = ')'
brackets = [leftP,rightP]

atomChar char =  not $ ((isSpace char) ||  (elem char brackets))
data Sexp a = Atom a | List [Sexp a] deriving (Show,Eq,Ord)

parseAtom :: GenParser Char st (Sexp String)
parseAtom = liftM Atom $ many1 $ satisfy atomChar

parseList :: GenParser Char st (Sexp String)
parseList =  liftM List $ many parseExpr

parseExpr = do { skipMany space 
               ; expr <- do {  parseAtom
                               <|> do { char leftP
                                      ; x <- parseList
                                      ; char rightP
                                      ; return x } } 
               ; skipMany space 
               ; return expr }
parseExprRest = do 
  expr <- parseExpr
  rest <- getInput
  return (expr,rest)

--fromString string | trace (take 100 string) False = undefined
fromString string = case parse parseExprRest "" string of
                      Right (expr,"")   -> [expr]
                      Right (expr,rest) -> expr:fromString rest
                      Left err -> error ("fromString: parse error: " ++ (show err))

prettyPrint = render . sexpDoc

sexpDoc (Atom a)  = text a
sexpDoc (List ss) = parens $ (hsep (fmap sexpDoc ss))

removeComments x = removeBetween (==';') (=='\n') x

removeBetween a b []  = []
removeBetween a b str = 
    case span (not . a) str of
      (prefix,[]) -> prefix
      (prefix,rest) -> case span (not . b) rest of
                         (comment,[]) -> prefix
                         (comment,_:rest'') -> prefix ++ removeBetween a b rest''
