{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import Control.Monad.Except
import Data.List

import Types

import qualified Data.Text as T

data Code = BoolValue Bool
          | NumValue Double
          | TextValue T.Text
          | VoidValue
          deriving Eq

instance Show Code where
  show (BoolValue b) = show b
  show (NumValue n) = let s = show n
                      in if isSuffixOf ".0" s then
                           take (length s - 2) s
                         else
                           s
  show (TextValue t) = show t
  show VoidValue = "nil"

isTruthy :: Code -> Bool
isTruthy VoidValue = False
isTruthy (BoolValue b) = b
isTruthy _ = True

isEqual :: Code -> Code -> Bool
isEqual VoidValue VoidValue = True
isEqual VoidValue _ = False
isEqual a b = a == b

numberOperandError :: (MonadError ErrorType m) => Token -> m Code
numberOperandError token = throwError $ RuntimeError token "Operands must be numbers"

evaluate :: (MonadError ErrorType m) => Expr -> m Code
evaluate (Literal (NumericLiteral n)) = pure $ NumValue n
evaluate (Literal (TextLiteral t)) = pure $ TextValue t
evaluate (Literal (BooleanLiteral b)) = pure $ BoolValue b
evaluate (Literal EmptyLiteral) = pure $ VoidValue
evaluate (Grouping expr) = evaluate expr
evaluate (Unary t@(Token Minus _ _ _) right) = do
  r <- evaluate right
  case r of
    (NumValue val) -> pure $ NumValue val
    _ -> throwError $ RuntimeError t "Operand must be a number"
evaluate (Unary (Token Bang _ _ _) right) = do
  r <- evaluate right
  pure $ BoolValue $ not $ isTruthy r
evaluate (Unary _ _) = pure $ VoidValue
evaluate (Binary left token right) = do
  l <- evaluate left
  r <- evaluate right
  case (token, l, r) of
    ((Token Minus _ _ _), NumValue ln, NumValue rn) -> pure $ NumValue $ ln - rn
    (t@(Token Minus _ _ _), _, _) -> numberOperandError t
    ((Token Slash _ _ _), NumValue ln, NumValue rn) -> pure $ NumValue $ ln / rn
    (t@(Token Slash _ _ _), _, _) -> numberOperandError t
    ((Token Star _ _ _), NumValue ln, NumValue rn) -> pure $ NumValue $ ln * rn
    (t@(Token Star _ _ _), _, _) -> numberOperandError t
    ((Token Plus _ _ _), NumValue ln, NumValue rn) -> pure $ NumValue $ ln + rn
    ((Token Plus _ _ _), TextValue ln, TextValue rn) -> pure $ TextValue $ ln <> rn
    (t@(Token Plus _ _ _), _, _) ->
      throwError $ RuntimeError t "Operands must be two numbers or two strings"
    ((Token Greater _ _ _), NumValue ln, NumValue rn) -> pure $ BoolValue $ ln > rn
    (t@(Token Greater _ _ _), _, _) -> numberOperandError t
    ((Token GreaterEqual _ _ _), NumValue ln, NumValue rn) -> pure $ BoolValue $ ln >= rn
    (t@(Token GreaterEqual _ _ _), _, _) -> numberOperandError t
    ((Token Less _ _ _), NumValue ln, NumValue rn) -> pure $ BoolValue $ ln < rn
    (t@(Token Less _ _ _), _, _) -> numberOperandError t
    ((Token LessEqual _ _ _), NumValue ln, NumValue rn) -> pure $ BoolValue $ ln <= rn
    (t@(Token LessEqual _ _ _), _, _) -> numberOperandError t
    ((Token BangEqual _ _ _), lval, rval) -> pure $ BoolValue $ not $ isEqual lval rval
    ((Token EqualEqual _ _ _), lval, rval) -> pure $ BoolValue $ isEqual lval rval
    (t, _, _) -> throwError $ RuntimeError t "Token in wrong place"

execute :: (MonadError ErrorType m, MonadIO m) => Stmt -> m ()
execute (Print expr) = do
  x <- evaluate expr
  liftIO $ print $ show x
  pure ()
execute (Expression expr) = void $ evaluate expr

interpret :: (MonadError ErrorType m, MonadIO m) => [Stmt] -> m ()
interpret ss = void $ traverse execute ss
