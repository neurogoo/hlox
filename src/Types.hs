{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Map as Map

data LiteralType = TextLiteral T.Text
                 | NumericLiteral Double
                 | BooleanLiteral Bool
                 | EmptyLiteral
                 deriving Show

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
               deriving (Eq, Ord, Show)

data Token = Token
  { _tokenType :: !TokenType
  , _lexeme    :: !T.Text
  , _literal   :: !(Maybe LiteralType)
  , _tokenLine :: !Int
  }

makeLenses ''Token

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

data Expr = Binary Expr Token Expr
          | Grouping Expr
          | Literal LiteralType
          | Unary Token Expr

parenthesize :: T.Text -> [Expr] -> T.Text
parenthesize name exprs = "(" <> name <> " " <> T.intercalate " " (printAst <$> exprs) <> " )"

printAst :: Expr -> T.Text
printAst (Binary left operator right) = parenthesize
    (operator ^. lexeme)
    [left, right]
printAst (Grouping expr) = parenthesize "group" [expr]
printAst (Literal EmptyLiteral) = "nil"
printAst (Literal (TextLiteral t)) = t
printAst (Literal (NumericLiteral n)) = T.pack $ show n
printAst (Literal (BooleanLiteral b)) = T.pack $ show b
printAst (Unary operator right) = parenthesize (operator ^. lexeme) [right]

testAstPrinter :: T.Text
testAstPrinter = printAst $ Binary
  (Unary (Token Minus "-" Nothing 1) (Literal $ NumericLiteral 123))
  (Token Star "*" Nothing 1)
  (Grouping $ Literal $ NumericLiteral 45.67)
