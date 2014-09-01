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


type Emb = U.Vector (Int, Double)

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

-- Embeddings can be in dense format or sparse format
-- dense:  0.1,0.2,0.5
-- sparse: 1:0.1,2:0.5

parseEmb :: Text.Text -> Emb 
parseEmb x = case Text.splitOn "," x of
  y:_ -> case Text.splitOn ":" y of
    [_,_] -> parseEmbSparse x
    _ -> parseEmbDense x
  _ -> error "GramLab.Morfette.Token.parseEmb: parse error"

parseEmbDense :: Text.Text -> Emb
parseEmbDense = U.fromList 
           . zip [0..]
           . map readDouble 
           . filter (not . Text.null)
           . Text.splitOn ","

parseEmbSparse :: Text.Text -> Emb
parseEmbSparse = U.fromList
                 . map parsePair
                 . filter (not . Text.null)
                 . Text.splitOn ","
                 
parsePair :: Text.Text -> (Int, Double)                 
parsePair x =
  case Text.splitOn ":" x of
    [f1, f2] -> (readInt f1, readDouble f2)
    _ -> error $ "GramLab.Morfette.Token.parsePair: parse error"

readDouble :: Text.Text -> Double
readDouble s = 
  case Text.double s of
    Right (d, "") -> d
    Left e        -> error $ "GramLab.Morfette.Token.readDouble: " ++ show e
    _             -> error $ "GramLab.Morfette.Token.readDouble: parse error"

readInt :: Text.Text -> Int
readInt s = 
  case Text.decimal s of 
    Right (d, "") -> d
    Left e -> error $ "GramLab.Morfette.Token.readInt: " ++ show e
    _      -> error $ "GramLab.Morfette.Token.readInt: parse error"
    
lowercaseToken :: Token -> Token
lowercaseToken t = token (lowercase (tokenForm t)) 
                         (tokenEmb t) 
                         (fmap lowercase (tokenLemma t)) 
                         (fmap lowercase (tokenPOS t))
  where lowercase = map toLower
        