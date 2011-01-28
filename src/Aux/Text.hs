{-# LANGUAGE OverloadedStrings 
  , TypeSynonymInstances 
  #-}
module Aux.Text 
    ( Txt
    , module Data.Text.Lazy
    , module Data.Text.Lazy.Encoding
    , splitOn
    , show
    , read
    , reads
    , readDouble
    , readInt
    , normalize
    , fromString
    , toString
    , getContents
    , readFile   
    , writeFile 
    , putStr    
    , putStrLn  
    , interact 
    , hPutStr 
    , hPutStrLn
    )
    where
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Text.Lazy.Read
import System.IO (Handle)
import qualified Data.Text.Lazy.IO as IO
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Strict
import Data.Binary
import qualified Aux.Utils as Utils
import Prelude hiding ( show
                      , reverse
                      , map
                      , read
                      , reads
                      , getContents
                      , readFile   
                      , writeFile 
                      , putStr    
                      , putStrLn  
                      , interact 
                      )
import qualified Prelude

type Txt = Text

instance Binary Txt where
    put = put . encodeUtf8
    get = fmap decodeUtf8 get

splitOn :: Char -> Txt -> [Txt]
splitOn c = Prelude.map pack . Utils.splitOn c . unpack

show :: (Show a) => a -> Txt
show = pack . Prelude.show

read :: (Read a) => Txt -> a
read = Prelude.read . unpack

reads :: (Read a) => Txt -> [(a,Txt)]
reads = Prelude.map (\ (r,s) -> (r,pack s)) . Prelude.reads . unpack

readDouble :: Txt -> Double 
readDouble t = case double t of
                 Right (d,"") -> d
                 Left err -> error err

readInt :: Txt -> Int
readInt t = case decimal t of
              Right (d,"") -> d
              Left err -> error err

fromString :: String -> Txt
fromString = pack

toString :: Txt -> String
toString = unpack

normalize :: Txt -> Txt
normalize = fromChunks . return . Strict.concat . toChunks

getContents :: IO Txt
getContents = decodeUtf8 `fmap` ByteString.getContents

readFile :: FilePath -> IO Txt
readFile  f = decodeUtf8 `fmap` ByteString.readFile f

writeFile :: FilePath -> Txt -> IO ()
writeFile f = ByteString.writeFile f . encodeUtf8

putStr :: Txt -> IO ()
putStr = ByteString.putStr . encodeUtf8

putStrLn :: Txt -> IO ()
putStrLn = ByteString.putStrLn . encodeUtf8

interact :: (Txt -> Txt) -> IO ()
interact f  = ByteString.interact (encodeUtf8 . f . decodeUtf8)

hPutStr :: Handle -> Txt -> IO ()
hPutStr h = ByteString.hPutStr h . encodeUtf8

hPutStrLn :: Handle -> Txt -> IO ()
hPutStrLn h s = do ByteString.hPutStr h . encodeUtf8 $ s
                   ByteString.hPutStr h . encodeUtf8 $ "\n"

