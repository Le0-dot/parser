{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Void
import Data.Aeson
import Text.Megaparsec
import qualified Data.Text as T


type Parser = Parsec Void T.Text

newtype File = File
    { functions :: [Function]
    } deriving (Show, Generic)

instance ToJSON File where
    toEncoding = genericToEncoding defaultOptions


data Function = Function
    { funcName :: Identifier
    , funcParams :: [FuncArg]
    , funcReturn :: Maybe Type
    , funcBody :: Block
    } deriving (Show, Generic)

instance ToJSON Function where
    toEncoding = genericToEncoding defaultOptions


data FuncArg = FuncArg
    { argName :: Identifier
    , argType :: Type
    } deriving (Show, Generic)

instance ToJSON FuncArg where
    toEncoding = genericToEncoding defaultOptions


type Type = Identifier


newtype Block = Block [Stmt] deriving (Show, Generic)

instance ToJSON Block where
    toEncoding = genericToEncoding defaultOptions


data Stmt
    = ReturnStmt             Expr
    | IgnoreResultStmt       Expr
    | VariableDefinitionStmt [VariableDefinition]
    deriving (Show, Generic)

instance ToJSON Stmt where
    toEncoding = genericToEncoding defaultOptions


data VariableDefinition = VariableDefinition
    { varName  :: Identifier
    , varType  :: Maybe Identifier
    , varValue :: Maybe Expr
    } deriving (Show, Generic)

instance ToJSON VariableDefinition where
    toEncoding = genericToEncoding defaultOptions


data Expr = Expr 
    { lhs :: Primary
    , rhs :: [BinOpRhs]
    } deriving (Show, Generic)

instance ToJSON Expr where
    toEncoding = genericToEncoding defaultOptions


data Primary
    = PrimaryId      Identifier
    | PrimaryLiteral Literal
    | PrimaryParens  Expr
    | PrimaryCall    Call
    deriving (Show, Generic)

instance ToJSON Primary where
    toJSON (PrimaryId i) = object ["type" .= ("id" :: String), "val" .= i]
    toJSON (PrimaryLiteral l) = object ["type" .= ("literal" :: String), "val" .= l]
    toJSON (PrimaryParens p) = object ["type" .= ("parens" :: String), "val" .= p]
    toJSON (PrimaryCall c) = object ["type" .= ("call" :: String), "val" .= c]

    toEncoding (PrimaryId i) = pairs ("type" .= ("id" :: String) <> "val" .= i)
    toEncoding (PrimaryLiteral l) = pairs ("type" .= ("literal" :: String) <> "val" .= l)
    toEncoding (PrimaryParens p) = pairs ("type" .= ("parens" :: String) <> "val" .= p)
    toEncoding (PrimaryCall c) = pairs ("type" .= ("call" :: String) <> "val" .= c)


data BinOpRhs = BinOpRhs
    { op :: OpIdentifier
    , rhsOperand :: Primary
    } deriving (Show, Generic)

instance ToJSON BinOpRhs where
    toEncoding = genericToEncoding defaultOptions


newtype Identifier = Identifier T.Text deriving (Show, Generic)

instance ToJSON Identifier where
    toEncoding = genericToEncoding defaultOptions


newtype OpIdentifier = OpIdentifier T.Text deriving (Show, Generic)

instance ToJSON OpIdentifier where
    toEncoding = genericToEncoding defaultOptions


data Literal
    = IntegerLiteral Integer
    | FloatLiteral   Double
    | CharLiteral    Char
    | StringLiteral  String
    | BoolLiteral    Bool
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Literal where
    toEncoding = genericToEncoding defaultOptions


data Call = Call
    { callable :: Identifier
    , callParams :: [Expr]
    } deriving (Show, Generic)

instance ToJSON Call where
    toEncoding = genericToEncoding defaultOptions
