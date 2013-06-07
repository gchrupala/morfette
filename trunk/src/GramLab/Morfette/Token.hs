{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Read as Text


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
  
parseToken :: Text.Text -> Token  
parseToken line =
  let str = Text.unpack
      t = 
        case Text.words line of 
          [form, embedding, lemma, pos] -> 
            token   (str form)   (Just (parseEmb embedding))  (Just $ str lemma)  (Just $ str pos)
          [form, lemma, pos]            -> 
            token   (str form)   Nothing                      (Just $ str lemma)  (Just $ str pos)
          [form, embedding]             -> 
            token   (str form)   (Just (parseEmb embedding))  Nothing       Nothing         
          [form]                        -> 
            token   (str form)   Nothing                      Nothing       Nothing
          []                            -> 
            nullToken
  in U.length (maybe U.empty id (tokenEmbedding t)) `seq` t

nullToken = token "" Nothing Nothing Nothing 
isNullToken t = t == nullToken

parseEmb :: Text.Text -> EMB
parseEmb = U.fromList 
           . filter ((/= 0.0) . snd) 
           . zip [0..] 
           . map readDouble 
           . filter (not . Text.null)
           . Text.splitOn ","

readDouble :: Text.Text -> Double
readDouble s = 
  case Text.double s of
    Right (d, "") -> d
    Left e        -> error $ "GramLab.Morfette.Token.readDouble: " ++ show e
    _             -> error $ "GramLab.Morfette.Token.readDouble: parse error"

lowercaseToken :: Token -> Token
lowercaseToken t = token (lowercase (tokenForm t)) 
                         (tokenEmbedding t) 
                         (fmap lowercase (tokenLemma t)) 
                         (fmap lowercase (tokenPOS t))
  where lowercase = map toLower
        