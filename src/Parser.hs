{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( skipWhiteSpace
    , file
    ) where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

import Types
import Lexeme


skipWhiteSpace :: Parser ()
skipWhiteSpace = L.space
    -- Skip 1 or more spaces
    (space1 <|> void eol)
    -- Skip C-like one line comments
    (L.skipLineComment "//")
    -- Skip C-list multi-line comments
    (L.skipBlockCommentNested "/*" "*/")

parseLine :: Parser a -> Parser a
parseLine = L.lexeme skipWhiteSpace

finalSymbol :: T.Text -> Parser T.Text
finalSymbol = L.symbol skipWhiteSpace

file :: Parser File
file = File <$> many function

function :: Parser Function
function = do
    let functionParam = VarDef <$> identifier <*> identifier
    void (parseKeyword "func")
    name <- dbg "func name" identifier
    params <- dbg "funciton parameters" $ parens $ many (functionParam <* commaOrFollowingRP)
    ret <- dbg "fucntion return type" $ optional identifier
    body <- dbg "funciton body" block
    return $ Function name params ret body

block :: Parser Block
block = Block <$> (finalSymbol "{" *> many stmt <* finalSymbol "}") <?> "block"

-- not used for now but will be needed with if-else 
ifBlock :: Parser Block
ifBlock = Block <$> (finalSymbol "{" *> many stmt <* symbol "}") <?> "block"

stmt :: Parser Stmt
stmt = dbg "stmt" $ parseLine $ label "statement" $ choice
    [ dbg "return" (ReturnStmt <$> (parseKeyword "return" *> expr) <?> "return statement")
    , dbg "ignore" $ IgnoreResultStmt <$> expr
    ] <* eol

expr :: Parser Expr
expr = Expr <$> primary <*> many binoprhs

primary :: Parser Primary
primary = dbg "primary" $ choice
    [ dbg "parens" $ PrimaryParens <$> parens expr
    , dbg "literal" $ PrimaryLiteral <$> literal
    , dbg "call" primaryWithId
    ]

binoprhs :: Parser BinOpRhs
binoprhs = dbg "binoprhs" $ BinOpRhs <$> opIdentifier <*> primary

-- either identifier of call (identifier with expressions in parenthesis)
primaryWithId :: Parser Primary
primaryWithId = do
    id <- identifier
    p <- optional $ parens $ many (expr <* commaOrFollowingRP)
    return $ case p of
        Just exprs -> PrimaryCall $ Call id exprs
        Nothing -> PrimaryId id
