{-# LANGUAGE OverloadedStrings #-}

module Lexeme 
    ( symbol
    , parseKeyword
    , identifier
    , opIdentifier
    , parens
    , literal
    ) where

import Data.Char (digitToInt, isAlphaNum, isDigit)
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

import Types


anyPred :: [a -> Bool] -> a -> Bool
anyPred ps x = any ($ x) ps

oneOfChar :: [Char] -> Char -> Bool
oneOfChar = anyPred . map (==)

textToInt :: T.Text -> Int
textToInt = T.foldl (\acc c -> acc * 10 + digitToInt c) 0

-- define parser for skipping spaces and comments
skipSpace :: Parser ()
skipSpace = L.space
    -- Skip 1 or more spaces or tabs
    hspace1
    -- Skip C-like one line comments
    (L.skipLineComment "//")
    -- Skip C-list multi-line comments
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol skipSpace

-- fucntions to parse keywords and separate them from identifiers
parseKeyword :: T.Text -> Parser ()
parseKeyword kw = lexeme $ do
    string kw
    notFollowedBy $ alphaNumChar <|> char '_' <|> char '\''

-- match identifiers such as [a-zA-Z_][a-zA-Z0-9_]*[']*
identifier :: Parser Identifier
identifier = lexeme $ label "identifier" $ do
    first <- letterChar <|> char '_'
    rest <- takeWhileP (Just "character, number, '_'") $ anyPred [isAlphaNum, (== '_')]
    prime <- takeWhileP (Just "prime mark") (== '\'')
    return $ Identifier $ first `T.cons` rest `T.append` prime

-- match oprator identifiers such as [a-zA-Z0-9!@#$%^&*-=_+<>?/|:]*
opIdentifier :: Parser OpIdentifier
opIdentifier = dbg "op id" $ lexeme $ label "operator" $ do
    let acceptable = "!@#$%^&*-=_+<>?/|:"
    let pred = (||) <$> oneOfChar acceptable <*> isAlphaNum
    OpIdentifier <$> takeWhile1P (Just $ acceptable ++ " or alpha-numeric") pred

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Literals
literal :: Parser Literal
literal = lexeme $ choice
    [ numeric
    , CharLiteral <$> charLiteral
    , StringLiteral <$> stringLiteral
    , BoolLiteral <$> bool -- backtraking to allow "falseSomething" as identifier
    ]

numeric :: Parser Literal
numeric = choice
    [ leadingZero
    , withScientific decimalOrFloat
    , withScientific $ FloatLiteral <$> fraction1
    ]

leadingZero :: Parser Literal
leadingZero = char '0' >> choice
    [ IntegerLiteral <$> binary
    , IntegerLiteral <$> octal
    , IntegerLiteral <$> hexadecimal
    , withScientific $ FloatLiteral <$> fraction
    , withScientific decimalOrFloat
    , pure $ IntegerLiteral 0
    ]

binary :: Parser Integer
binary = char 'b' >> L.binary <?> "binary literal"

octal :: Parser Integer
octal = char 'o' >> L.octal <?> "octal literal"

hexadecimal :: Parser Integer
hexadecimal = char 'x' >> L.hexadecimal <?> "hexadecimal literal"

scientific :: Parser Integer
scientific = char 'e' >> L.signed skipSpace L.decimal <?> "scientific notaion power"

decimalOrFloat :: Parser Literal
decimalOrFloat = do
    decimal <- L.decimal <?> "decimal literal"
    frac <- optional fraction
    return $ case frac of
        Just f -> FloatLiteral (fromInteger decimal + f)
        Nothing -> IntegerLiteral decimal

fraction :: Parser Double
fraction = label "fractional part" $ do
    void (char '.')
    digits <- takeWhileP (Just "fraction digit") isDigit
    let l = T.length digits
    return $ if l == 0
        then 0
        else fromIntegral (textToInt digits) / 10^l

fraction1 :: Parser Double
fraction1 = label "fractional part" $ do
    void (char '.')
    digits <- takeWhile1P (Just "fraction digit") isDigit
    return (fromIntegral (textToInt digits) / 10 ^ T.length digits)

withScientific :: Parser Literal -> Parser Literal
withScientific parser = do
    value <- parser
    e <- optional scientific
    return $ case e of
        Just power -> FloatLiteral $ case value of
            IntegerLiteral i -> fromInteger i * 10 ^^ power
            FloatLiteral f -> f * 10 ^^ power
        Nothing -> value

bool :: Parser Bool
bool = False <$ parseKeyword "false" <|> True <$ parseKeyword "true"

charLiteral :: Parser Char
charLiteral = between quote quote L.charLiteral <?> "char literal"
    where quote = char '\''

stringLiteral :: Parser String
stringLiteral = dquote *> manyTill L.charLiteral dquote <?> "string literal"
    where dquote = char '"'
