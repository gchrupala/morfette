{-# LANGUAGE OverloadedStrings #-}

module GramLab.Morfette.Token ( Token
                              , Sentence
                              , Emb
                              , tokenForm
                              , tokenEmb
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


type Emb = U.Vector Double

data Token      = Token { tokenForm :: String
                        , tokenEmb :: Maybe Emb
                        , tokenLemma :: Maybe String
                        , tokenPOS :: Maybe String
                        } deriving (Show, Eq)
type Sentence   = [Token]

token :: String -> Maybe Emb -> Maybe String -> Maybe String -> Token
token f e l p = 
  Token { tokenForm = f, tokenEmb = e, tokenLemma = l, tokenPOS = p }
  
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
  in U.length (maybe U.empty id (tokenEmb t)) `seq` t

nullToken = token "" Nothing Nothing Nothing 
isNullToken t = t == nullToken

parseEmb :: Text.Text -> Emb
parseEmb = U.fromList 
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
                         (tokenEmb t) 
                         (fmap lowercase (tokenLemma t)) 
                         (fmap lowercase (tokenPOS t))
  where lowercase = map toLower
        