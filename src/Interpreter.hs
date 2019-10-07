{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Interpreter where

import Control.Monad.Except
import Data.List
import Control.Monad.State.Strict
import Control.Lens
import Data.Maybe (fromMaybe)
import Debug.Trace

import Types

import qualified Data.Text as T
import qualified Data.Map as Map

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
  show (TextValue t) = T.unpack t
  show VoidValue = "nil"

data Environment = Environment
  { values    :: !(Map.Map T.Text Code)
  , enclosing :: !(Maybe Environment)
  } deriving Show

class MonadEnvironment m where
  define      :: T.Text -> Code -> m ()
  getValue    :: Token -> m Code
  assignValue :: Token -> Code -> m ()
  getEnv      :: m Environment
  setEnv      :: Environment -> m ()

instance ( Monad m
         , MonadError ErrorType m) => MonadEnvironment (StateT Environment m) where
  define name code = do
    (Environment valMap enc) <- get
    put (Environment (Map.insert name code valMap) enc)
  getValue token = do
    let lookupEnv (Environment valMap env) =
          case Map.lookup (token ^. lexeme) valMap of
            Just code -> pure code
            Nothing -> case env of
              Just env' -> lookupEnv env'
              Nothing -> throwError $
                RuntimeError token ("Undefined variable '" <> token ^. lexeme <> "'.")
    env <- get
    lookupEnv env
  assignValue token code = do
    let putVal (Environment valMap env) =
          case Map.lookup (token ^. lexeme) valMap of
            Just _ -> pure $ Environment (Map.insert (token ^. lexeme) code valMap) env
            Nothing -> case env of
              Just env' -> do
                val <- putVal env'
                pure $ Environment valMap (Just val)
              Nothing -> throwError $ RuntimeError token ("Undefined variable '" <> token ^. lexeme <> "'.")
    env <- get
    finalVal <- putVal env
    put finalVal
  getEnv = get
  setEnv = put

isTruthy :: Code -> Bool
isTruthy VoidValue = False
isTruthy (BoolValue b) = b
isTruthy _ = True

numberOperandError :: (MonadError ErrorType m) => Token -> m Code
numberOperandError token = throwError $ RuntimeError token "Operands must be numbers"

evaluate :: (MonadEnvironment m, MonadError ErrorType m) => Expr -> m Code
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
evaluate (Variable token) = getValue token
evaluate (Assign token expr) = do
  value <- evaluate expr
  assignValue token value
  pure value
evaluate (Logical left operator right) = do
  l <- evaluate left
  let res =
        case operator ^. tokenType of
          Or -> if isTruthy l then Just l else Nothing
          _ -> if not $ isTruthy l then Just l else Nothing
  maybe (evaluate right) pure res

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
    ((Token BangEqual _ _ _), lval, rval) -> pure $ BoolValue $ not $ lval == rval
    ((Token EqualEqual _ _ _), lval, rval) -> pure $ BoolValue $ lval == rval
    (t, _, _) -> throwError $ RuntimeError t "Token in wrong place"

execute :: (MonadError ErrorType m, MonadEnvironment m, MonadIO m) => Stmt -> m ()
execute (Block ss) = do
  env <- getEnv
  setEnv $ Environment Map.empty $ Just env
  void $ traverse execute ss `catchError`
    \e -> getEnv >>= setEnv . fromMaybe env . enclosing >> throwError e
  env' <- getEnv
  setEnv $ fromMaybe env' $ enclosing env'
execute (Print expr) = do
  x <- evaluate expr
  liftIO $ print $ show x
execute (Expression expr) = void $ evaluate expr
execute (Var token initializer) = do
  value <- traverse evaluate initializer
  define (token ^. lexeme) $ fromMaybe VoidValue value
  pure ()
execute (If expr thenstmt elsestmt) = do
  e <- evaluate expr
  if isTruthy e then
    execute thenstmt
  else
    void $ traverse execute elsestmt
execute (While condition body) = do
  c <- evaluate condition
  when (isTruthy c) $ do
    execute body
    execute $ While condition body
execute (ReplExpression expr) = execute $ Print expr

interpret :: (MonadError ErrorType m, MonadEnvironment m, MonadIO m) => [Stmt] -> m ()
interpret ss = void $ traverse execute ss
