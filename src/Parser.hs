{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
module Parser where

import Control.Lens
import Data.Maybe (listToMaybe)
import Control.Monad.State.Strict
import Debug.Trace
import Control.Monad.Except

import Types

import qualified Data.Text as T

data Parser = Parser
  { _tokens :: ![Token]
  , _prev   :: !(Maybe Token)
  , _errors :: [(Token,T.Text)]
  }

makeLenses ''Parser

class MonadParser m where
  match      :: TokenType -> m (Either Token ())
  peek       :: m Token
  advance    :: m Token
  parseError :: (Token, T.Text) -> m ()

instance (Monad m) => MonadParser (StateT Parser m) where
  match tt = do
    ts <- use tokens
    case listToMaybe ts of
      Just t -> if t ^. tokenType == tt then do
        tokens %= drop 1
        pure $ Left t
        else pure $ Right ()
      Nothing -> pure $ Right ()
  peek = do
    ts <- use tokens
    pure $ head ts
  advance = do
    ts <- use tokens
    case ts of
      [x] -> pure x
      x : _ -> do
        tokens %= drop 1
        pure x
      _ -> error "This should not happen"
  parseError err = errors <>= [err]

type ParserConstrains m = (Monad m, MonadParser m, MonadError (Token, T.Text) m)

check :: ParserConstrains m => TokenType -> m Bool
check tt = do
  p <- peek
  pure $ p ^. tokenType == tt

isAtEnd :: ParserConstrains m => m Bool
isAtEnd = do
  p <- peek
  pure $ p ^. tokenType == Eof

throwParseError :: (MonadParser m, MonadError (Token,T.Text) m) => (Token, T.Text) -> m a
throwParseError err = do
  parseError err
  throwError err

consume :: ParserConstrains m => TokenType -> T.Text -> m Token
consume tt errorMessage = do
  m <- peek
  if m ^. tokenType == tt then
    advance
  else do
    throwParseError (m, errorMessage)

anyMatches :: ParserConstrains m => [TokenType] -> m (Either Token ())
anyMatches tokenTypes = foldM (\b a -> case b of
                                  Right () -> match a
                                  Left t -> pure $ Left t) (Right ()) tokenTypes

findUntil :: ParserConstrains m => m Expr -> [TokenType] -> m Expr
findUntil f tokenTypes = do
    startVal <- f
    go tokenTypes startVal
  where
    go tokenTypes' expr = do
      m <- anyMatches tokenTypes'
      case m of
        Left token -> do
          let operator = token
          right <- f
          go tokenTypes $ Binary expr operator right
        Right () ->
          pure expr

expression :: ParserConstrains m => m Expr
expression = assignment

assignment :: ParserConstrains m => m Expr
assignment = do
  expr <- equality
  m <- match Equal
  case m of
    Left equals -> do
      value <- assignment
      case expr of
        Variable name -> pure $ Assign name value
        _ -> throwParseError (equals, "Invalid assignment target.") --TODO: just report error, not throw
    Right () -> pure expr

declaration :: ParserConstrains m => m Stmt
declaration = go `catchError` const synchronize
  where
    go = do
      m <- match VAR
      case m of
        Left _ -> varDeclaration
        _ -> statement

varDeclaration :: ParserConstrains m => m Stmt
varDeclaration = do
  name <- consume Identifier "Expect variable name."
  m <- match Equal
  initializer <-
    case m of
      Left _ -> Just <$> expression
      Right () -> pure $ Nothing
  void $ consume Semicolon "Expect ';' after variable declaration."
  pure $ Var name initializer

statement :: ParserConstrains m => m Stmt
statement = do
  x <- match PRINT
  case x of
    Left _ -> printStatement
    _ -> do
      x' <- match LeftBrace
      case x' of
        Left _ -> Block <$> block
        _ -> expressionStatement

block :: ParserConstrains m => m [Stmt]
block = do
  let getStatements ss = do
        a <- check RightBrace
        b <- isAtEnd
        if not a && not b then do
          s <- declaration
          getStatements (ss <> [s])
        else
          pure ss
  ss <- getStatements []
  void $ consume RightBrace "Expect '}' after block."
  pure $ ss

printStatement :: ParserConstrains m => m Stmt
printStatement = do
  value <- expression
  _ <- consume Semicolon "Expect ';' after value."
  pure $ Print value

expressionStatement :: ParserConstrains m => m Stmt
expressionStatement = do
  value <- expression
  _ <- consume Semicolon "Expect ';' after expression."
  pure $ Expression value

equality :: ParserConstrains m => m Expr
equality = findUntil comparison [BangEqual,EqualEqual]

comparison :: ParserConstrains m => m Expr
comparison = findUntil addition [Greater, GreaterEqual, Less, LessEqual]

addition :: ParserConstrains m => m Expr
addition = findUntil multiplication [Minus, Plus]

multiplication :: ParserConstrains m => m Expr
multiplication = findUntil unary [Slash, Star]

unary :: ParserConstrains m => m Expr
unary = do
  matches <- anyMatches [Bang, Minus]
  case matches of
    Left tt -> do
      let operator = tt
      right <- unary
      pure $ Unary operator right
    Right () -> primary

primary :: ParserConstrains m => m Expr
primary = do
  x <- runExceptT $ ExceptT ifFalse
    >> ExceptT ifTrue
    >> ExceptT ifNil
    >> ExceptT ifNumberOrString
    >> ExceptT ifIdentifier
    >> ExceptT ifLeftParen
  case x of
    Left expr -> pure expr
    Right _ -> do
      p <- peek
      throwParseError (p, "Couldn't find matching primary for token")
  where
    ifIs tt exprf = do
      m <- match tt
      case m of
        Left t -> pure $ Left $ exprf t
        Right _ -> pure $ Right ()
    ifFalse = ifIs FALSE (const $ Literal $ BooleanLiteral False)
    ifTrue = ifIs TRUE (const $ Literal $ BooleanLiteral True)
    ifNil = ifIs Nil (const $ Literal $ EmptyLiteral)
    ifNumberOrString = do
      matches <- anyMatches [Number, String]
      case matches of
        Left (Token _ _ (Just l) _ ) -> pure $ Left $ Literal l
        _ -> pure $ Right ()
    ifIdentifier = ifIs Identifier Variable
    ifLeftParen = do
      m <- match LeftParen
      case m of
        Left _ -> do
          expr <- expression
          _ <- consume RightParen "Expect ')' after expression."
          pure $ Left $ Grouping expr
        Right () -> pure $ Right ()

parse :: [Token] -> Either (Token, T.Text) [Stmt]
parse ts = flip evalStateT (Parser ts Nothing []) $ program []
  where
    program ss = do
      p <- peek
      case p of
        (Token Eof _ _ _) -> pure ss
        _ -> do
          s <- declaration
          program (ss <> [s])

--TODO: add problem error reporting for this
synchronize :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Stmt
synchronize = do
    previous <- advance
    go $ previous ^. tokenType
  where
    go previous = do
      case previous of
        Semicolon -> pure EmptyStatement
        _ -> do
          current <- peek
          case current ^. tokenType of
            Class -> pure EmptyStatement
            Fun -> pure EmptyStatement
            VAR -> pure EmptyStatement
            For -> pure EmptyStatement
            If -> pure EmptyStatement
            While -> pure EmptyStatement
            PRINT -> pure EmptyStatement
            Return -> pure EmptyStatement
            _ -> do
              t'' <- advance
              go $ t'' ^. tokenType
