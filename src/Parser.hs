{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Control.Lens
import Data.Maybe (fromMaybe, fromJust, listToMaybe)
import Control.Monad.State.Strict
import Control.Monad.Reader.Class
import Debug.Trace
import Control.Monad.Except

import Types

import qualified Data.Text as T

data Parser = Parser
  { _tokens :: ![Token]
  , _prev   :: !(Maybe Token)
  }

makeLenses ''Parser

class MonadParser m where
  match      :: TokenType -> m (Either Token ())
  peek       :: m Token
  advance    :: m Token

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

consume :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => TokenType -> T.Text -> m Token
consume tt errorMessage = do
  m <- peek
  if m ^. tokenType == tt then
    advance
  else do
    throwError (m, errorMessage)

anyMatches :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => [TokenType] -> m (Either Token ())
anyMatches tokenTypes = foldM (\b a -> case b of
                                  Right () -> match a
                                  Left t -> pure $ Left t) (Right ()) tokenTypes

findUntil :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr -> [TokenType] -> m Expr
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

expression :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr
expression = equality

equality :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr
equality = findUntil comparison [BangEqual,EqualEqual]

comparison :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr
comparison = findUntil addition [Greater, GreaterEqual, Less, LessEqual]

addition :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr
addition = findUntil multiplication [Minus, Plus]

multiplication :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr
multiplication = findUntil unary [Slash, Star]

unary :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr
unary = do
  matches <- anyMatches [Bang, Minus]
  case matches of
    Left tokenType -> do
      let operator = tokenType
      right <- unary
      pure $ Unary operator right
    Right () -> primary

primary :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m Expr
primary = do
  x <- ifFalse
  pure $ fromMaybe (Literal EmptyLiteral) x
  where
    ifFalse = do
      m <- match FALSE
      case m of
        Left _ -> pure $ Just $ Literal $ BooleanLiteral False
        Right () -> ifTrue
    ifTrue = do
      m <- match TRUE
      case m of
        Left _ -> pure $ Just $ Literal $ BooleanLiteral True
        Right () -> ifNil
    ifNil = do
      m <- match Nil
      case m of
        Left _ -> pure $ Just $ Literal $ EmptyLiteral
        Right () -> ifNumberOrString
    ifNumberOrString = do
      matches <- anyMatches [Number, String]
      case matches of
        Left p -> pure $ Literal <$> (p ^. literal)
        Right () -> ifLeftParen
    ifLeftParen = do
      m <- match LeftParen
      case m of
        Left _ -> do
          expr <- expression
          _ <- consume RightParen "Expect ')' after expression."
          pure $ Just $ Grouping expr
        Right () -> pure Nothing

parse :: [Token] -> Either (Token, T.Text) Expr
parse ts = flip evalStateT (Parser ts Nothing) $ expression

synchronize :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m ()
synchronize = do
    previous <- advance
    go $ previous ^. tokenType
  where
    go previous = do
      case previous of
        Semicolon -> pure ()
        _ -> do
          current <- peek
          case current ^. tokenType of
            Class -> pure ()
            Fun -> pure ()
            Var -> pure ()
            For -> pure ()
            If -> pure ()
            While -> pure ()
            Print -> pure ()
            Return -> pure ()
            _ -> do
              t'' <- advance
              go $ t'' ^. tokenType
