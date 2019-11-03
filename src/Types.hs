{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import Data.Maybe (catMaybes)
import Data.List
import Data.IORef
import Control.Monad.Trans

import qualified Data.Text as T
import qualified Data.Map as Map

data LiteralType = TextLiteral T.Text
                 | NumericLiteral Double
                 | BooleanLiteral Bool
                 | EmptyLiteral
                 deriving (Eq, Ord, Show)

data CompilerMode = Repl
                  | Compile

data FunctionType = FunctionType deriving Eq

instance Show FunctionType where
  show FunctionType = "function"

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
               | IDENTIFIER -- literals
               | String
               | Number
               | And -- keywords
               | CLASS
               | Else
               | FALSE
               | FUN
               | For
               | IF
               | Nil
               | Or
               | PRINT
               | RETURN
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
  } deriving (Eq, Ord)

makeLenses ''Token

keywords :: Map.Map T.Text TokenType
keywords = Map.fromList
  [ ("and", And)
  , ("class", CLASS)
  , ("else", Else)
  , ("false", FALSE)
  , ("for", For)
  , ("fun", FUN)
  , ("if", IF)
  , ("nil", Nil)
  , ("or", Or)
  , ("print", PRINT)
  , ("return", RETURN)
  , ("super", Super)
  , ("this", This)
  , ("true", TRUE)
  , ("var", VAR)
  , ("while", WHILE)
  ]

data Expr = Assign Token Expr
          | Binary Expr Token Expr
          | Call Expr Token [Expr]
          | Grouping Expr
          | Lambda [Token] [Stmt] --need some token for errors?
          | Literal LiteralType
          | Logical Expr Token Expr
          | Unary Token Expr
          | Variable Token
          deriving (Eq, Ord, Show)

data Stmt = Block [Stmt]
          | Expression Expr
          | Function Token [Token] [Stmt]
          | If Expr Stmt (Maybe Stmt)
          | Print Expr
          | Return Token (Maybe Expr)
          | Var Token (Maybe Expr)
          | While Expr Stmt
          | ReplExpression Expr
          deriving (Eq, Ord, Show)

data Environment = GlobalEnvironment (IORef (Map.Map T.Text Code))
                 | Environment (IORef (Map.Map T.Text Code)) Environment

applyToEnv :: (MonadIO m) => (Map.Map T.Text Code -> Map.Map T.Text Code) -> Environment -> m ()
applyToEnv f (GlobalEnvironment valMap) = liftIO $ modifyIORef' valMap f
applyToEnv f (Environment valMap _) = liftIO $ modifyIORef' valMap f

liftToEnv :: (MonadIO m) => (Map.Map T.Text Code -> a) -> Environment -> m a
liftToEnv f (GlobalEnvironment valMap) = f <$> (liftIO $ readIORef valMap)
liftToEnv f (Environment valMap _) = f <$> (liftIO $ readIORef valMap)

enclosingEnv :: Environment -> Environment
enclosingEnv (Environment _ enc) = enc
enclosingEnv e = e

data LoxFunction = LoxFunction (Maybe Token) [Token] [Stmt] Environment
                 | NativeFunction Int ([Code] -> IO Code) T.Text

instance Show LoxFunction where
  show (LoxFunction (Just n) _ _ _ ) = "<fn " <> T.unpack (n ^. lexeme) <> ">"
  show (LoxFunction _ _ _ _) = "<anonymous fn>"
  show (NativeFunction _ _ s) = T.unpack s

-- TODO is this needed?
instance Eq LoxFunction where
  _ == _ = False

data Code = BoolValue Bool
          | NumValue Double
          | TextValue T.Text
          | VoidValue
          | FunctionValue LoxFunction
          deriving Eq

instance Show Code where
  show (BoolValue b) = show b
  show (NumValue n) = let s = show n
                      in if isSuffixOf ".0" s then
                           take (length s - 2) s
                         else
                           s
  show (TextValue t) = T.unpack t
  show (FunctionValue func) = show func
  show VoidValue = "nil"

data ErrorType = ScanError Int T.Text
               | ParserError Token T.Text
               | RuntimeError Token T.Text
               | ReturnValue Code

parenthesize :: T.Text -> [Expr] -> T.Text
parenthesize name exprs = "(" <> name <> " " <> T.intercalate " " (printAst <$> exprs) <> " )"

parenthesizeStmt :: T.Text -> [Stmt] -> T.Text
parenthesizeStmt name exprs = "(" <> name <> " " <> T.intercalate " " (printAstStmt <$> exprs) <> " )"

printAstStmt :: Stmt -> T.Text
printAstStmt (Expression expr) = printAst expr
printAstStmt (Function fname arguments body) =
  parenthesizeStmt
  ("fun " <> fname ^. lexeme <> (T.intercalate " " $ fmap (^. lexeme) arguments))
  body
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
printAstStmt (Return _ expr) = parenthesize "return" $ maybe [] pure expr

printAst :: Expr -> T.Text
printAst (Binary left operator right) = parenthesize
    (operator ^. lexeme)
    [left, right]
printAst (Call callee _ arguments) = parenthesize (printAst callee) arguments
printAst (Grouping expr) = parenthesize "group" [expr]
printAst (Lambda arguments body) =
  parenthesizeStmt
  ("fun " <> (T.intercalate " " $ fmap (^. lexeme) arguments))
  body
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
