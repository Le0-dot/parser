module Main where

import Text.Megaparsec
import qualified Data.Text.IO as T
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS

import Types
import Lexeme
import Parser

main :: IO ()
main = do
    [f] <- getArgs
    c <- T.readFile f
    let out = parse (between skipWhiteSpace eof file) f c
    case out of
        Left err -> putStrLn $ errorBundlePretty err
        Right out -> BS.writeFile (f ++ ".ast.json") $ encodePretty out
