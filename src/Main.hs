module Main where

import Text.Megaparsec
import qualified Data.Text.IO as T
import Data.Aeson (encodeFile)
import System.Environment (getArgs)

import Types
import Lexeme
import Parser

-- main function to run parser
main :: IO ()
main = do
    [f] <- getArgs
    c <- T.readFile f
    let out = parse (between skipWhiteSpace eof file) f c
    case out of
        Left err -> putStrLn $ errorBundlePretty err
        Right out -> encodeFile (f ++ ".ast.json") out
