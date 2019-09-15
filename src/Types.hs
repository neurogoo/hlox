{-# LANGUAGE OverloadedStrings #-}
module Types where

import qualified Data.Text as T
import qualified Data.Map as Map

data LiteralType = TextLiteral T.Text
                 | NumericLiteral Double
                 deriving Show

data Token = Token
  { tokenType :: !TokenType
  , lexeme    :: !T.Text
  , literal   :: !(Maybe LiteralType)
  , tokenLine :: !Int
  }

instance Show Token where
  show (Token tokenType lexeme literal _) =
    show tokenType <> " " <> T.unpack lexeme <> " " <> show literal

data TokenType = LeftParen -- Single character tokens
               | RightParen
               | LeftBrace
               | RightBrace
               | Comma
               | Dot
               | Minus
               | Plus
               | Semicolon
               | Slash
               | Star
               | Bang -- One or two character tokens
               | BangEqual
               | Equal
               | EqualEqual
               | Greater
               | GreaterEqual
               | Less
               | LessEqual
               | Identifier -- literals
               | String
               | Number
               | And -- keywords
               | Class
               | Else
               | FALSE
               | Fun
               | For
               | If
               | Nil
               | Or
               | Print
               | Return
               | Super
               | This
               | TRUE
               | Var
               | While
               | Eof
               deriving Show

keywords :: Map.Map T.Text TokenType
keywords = Map.fromList
  [ ("and", And)
  , ("class", Class)
  , ("else", Else)
  , ("false", FALSE)
  , ("for", For)
  , ("fun", Fun)
  , ("if", If)
  , ("nil", Nil)
  , ("or", Or)
  , ("print", Print)
  , ("return", Return)
  , ("super", Super)
  , ("this", This)
  , ("true", TRUE)
  , ("var", Var)
  , ("while", While)
  ]
