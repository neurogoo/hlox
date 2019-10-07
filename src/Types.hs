{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import Data.Maybe (catMaybes)

import qualified Data.Text as T
import qualified Data.Map as Map

data LiteralType = TextLiteral T.Text
                 | NumericLiteral Double
                 | BooleanLiteral Bool
                 | EmptyLiteral
                 deriving Show

data ErrorType = ScanError Int T.Text
               | ParserError Token T.Text
               | RuntimeError Token T.Text

data CompilerMode = Repl
                  | Compile

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
               | IF
               | Nil
               | Or
               | PRINT
               | Return
               | Super
               | This
               | TRUE
               | VAR
               | WHILE
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
  , ("if", IF)
  , ("nil", Nil)
  , ("or", Or)
  , ("print", PRINT)
  , ("return", Return)
  , ("super", Super)
  , ("this", This)
  , ("true", TRUE)
  , ("var", VAR)
  , ("while", WHILE)
  ]

data Expr = Assign Token Expr
          | Binary Expr Token Expr
          | Grouping Expr
          | Literal LiteralType
          | Logical Expr Token Expr
          | Unary Token Expr
          | Variable Token
          deriving Show

data Stmt = Block [Stmt]
          | Expression Expr
          | If Expr Stmt (Maybe Stmt)
          | Print Expr
          | Var Token (Maybe Expr)
          | While Expr Stmt
          | ReplExpression Expr
          deriving Show

parenthesize :: T.Text -> [Expr] -> T.Text
parenthesize name exprs = "(" <> name <> " " <> T.intercalate " " (printAst <$> exprs) <> " )"

parenthesizeStmt :: T.Text -> [Stmt] -> T.Text
parenthesizeStmt name exprs = "(" <> name <> " " <> T.intercalate " " (printAstStmt <$> exprs) <> " )"

printAstStmt :: Stmt -> T.Text
printAstStmt (Expression expr) = printAst expr
printAstStmt (Print expr) = parenthesize "print" [expr]
printAstStmt (Var (Token _ lexeme' _ _) expr) = parenthesize ("var " <> lexeme') $ catMaybes [expr]
printAstStmt (Block ss) = "'(" <> T.intercalate " " (printAstStmt <$> ss) <> ")"
printAstStmt (If expr stmt1 stmt2) =
  "( if "
  <> T.intercalate " " [ printAst expr
                       , printAstStmt stmt1
                       , maybe "" printAstStmt stmt2]
  <> ")"
printAstStmt (While expr body) = "( while " <> T.intercalate " " [ printAst expr
                                                                 , printAstStmt body] <> ")"
printAstStmt (ReplExpression expr) = printAst expr

printAst :: Expr -> T.Text
printAst (Binary left operator right) = parenthesize
    (operator ^. lexeme)
    [left, right]
printAst (Grouping expr) = parenthesize "group" [expr]
printAst (Literal EmptyLiteral) = "nil"
printAst (Literal (TextLiteral t)) = t
printAst (Literal (NumericLiteral n)) = T.pack $ show n
printAst (Literal (BooleanLiteral b)) = T.pack $ show b
printAst (Logical left operator right) = parenthesize
    (operator ^. lexeme)
    [left, right]
printAst (Unary operator right) = parenthesize (operator ^. lexeme) [right]
printAst (Variable token) = token ^. lexeme
printAst (Assign token expr) = parenthesize ("= " <> token ^. lexeme) [expr]

testAstPrinter :: T.Text
testAstPrinter = printAst $ Binary
  (Unary (Token Minus "-" Nothing 1) (Literal $ NumericLiteral 123))
  (Token Star "*" Nothing 1)
  (Grouping $ Literal $ NumericLiteral 45.67)
