module GramLab.Morfette.Token ( Token
                              , Sentence
                              , tokenForm
                              , tokenEmbedding
                              , tokenLemma
                              , tokenPOS
                              , parseToken
                              , nullToken
                              , isNullToken 
                              , lowercaseToken
                              )
                               
where
import qualified Data.Vector.Unboxed as U
import GramLab.Utils
import Data.Char
import Debug.Trace

type EMB = U.Vector (Int, Double)

data Token      = Token { tokenForm :: String
                        , tokenEmbedding :: Maybe EMB
                        , tokenLemma :: Maybe String
                        , tokenPOS :: Maybe String
                        } deriving (Show, Eq)
type Sentence   = [Token]

token :: String -> Maybe EMB -> Maybe String -> Maybe String -> Token
token f e l p = 
  Token { tokenForm = f, tokenEmbedding = e, tokenLemma = l, tokenPOS = p }
  
parseToken line =
  let t = 
        case words line of 
          [form, embedding, lemma, pos] -> 
            token   form   (Just (parseEmb embedding))  (Just lemma)  (Just pos)
          [form, lemma, pos]            -> 
            token   form   Nothing                      (Just lemma)  (Just pos)
          [form, embedding]             -> 
            token   form   (Just (parseEmb embedding))  Nothing       Nothing         
          [form]                        -> 
            token   form   Nothing                      Nothing       Nothing
          []                            -> 
            nullToken
  in U.length (maybe U.empty id (tokenEmbedding t)) `seq` t

nullToken = token "" Nothing Nothing Nothing 
isNullToken t = t == nullToken

parseEmb :: String -> EMB
parseEmb = U.fromList . filter ((/= 0.0) . snd) . zip [0..] . map read . splitOn ','

lowercaseToken :: Token -> Token
lowercaseToken t = token (lowercase (tokenForm t)) 
                         (tokenEmbedding t) 
                         (fmap lowercase (tokenLemma t)) 
                         (fmap lowercase (tokenPOS t))
  where lowercase = map toLower
        