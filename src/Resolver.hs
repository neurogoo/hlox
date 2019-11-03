{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds   #-}
module Resolver where

import Types
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Lens
import Control.Monad.Except
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Text as T

data ResolverState = ResolverState
  { scopes :: [Map.Map T.Text Bool]
  , currentFunction :: Maybe FunctionType
  }

type ResolverConstrains m =
  ( MonadState ResolverState m
  , MonadError ErrorType m
  , MonadWriter (Map.Map Expr Int) m)

resolveStmt :: ResolverConstrains m => Stmt -> m ()
resolveStmt (Block statements) = do
  beginScope
  void $ traverse resolveStmt statements
  endScope
  pure ()
resolveStmt (Var name initializer) = do
  declare name
  forM_ initializer $ resolveExpr
  define name
  pure ()
resolveStmt (Function name params body) = do
  declare name
  define name
  resolveFunction params body FunctionType
resolveStmt (Expression expr) = resolveExpr expr
resolveStmt (If condition thenBranch elseBranch) = do
  resolveExpr condition
  resolveStmt thenBranch
  void $ traverse resolveStmt elseBranch
resolveStmt (Print expr) = resolveExpr expr
resolveStmt (Return keyword value) = do
  ResolverState _ ftype <- get
  when (ftype == Nothing) $ throwError $ ParserError keyword "Cannot return from top-level code."
  void $ traverse resolveExpr value
resolveStmt (While condition body) = do
  resolveExpr condition
  resolveStmt body
resolveStmt (ReplExpression expr) = resolveExpr expr

resolveFunction :: ResolverConstrains m
                => [Token]
                -> [Stmt]
                -> FunctionType
                -> m ()
resolveFunction params body functionType = do
  ResolverState s enclosingFunction <- get
  put $ ResolverState s (Just functionType)
  beginScope
  forM_ params $ \param -> declare param >> define param
  void $ traverse resolveStmt body
  endScope
  put $ ResolverState s enclosingFunction

resolveExpr :: ResolverConstrains m => Expr -> m ()
resolveExpr (Variable name) = do
  (ResolverState ss _) <- get
  case listToMaybe ss of
    Just s | Map.lookup (name ^. lexeme) s == Just False ->
             throwError $ ParserError name "Cannot read local variable in its own initializer."
    _ -> pure ()
  resolveLocal (Variable name) name
  pure ()
resolveExpr (Assign name value) = do
  resolveExpr value
  resolveLocal (Assign name value) name
resolveExpr (Binary left _ right) = do
  resolveExpr left
  resolveExpr right
resolveExpr (Call callee _ arguments ) = do
  resolveExpr callee
  void $ traverse resolveExpr arguments
resolveExpr (Grouping expr) = resolveExpr expr
resolveExpr (Literal _) = pure ()
resolveExpr (Logical left _ right) = do
  resolveExpr left
  resolveExpr right
resolveExpr (Unary _ right) = resolveExpr right

resolveLocal :: ResolverConstrains m => Expr -> Token -> m ()
resolveLocal expr name = do
  ResolverState ss ftype <- get
  let scopesLength = length ss
  let go i (ResolverState (s : ss') ftype) | Map.lookup (name ^. lexeme) s /= Nothing =
                                             resolve expr (scopesLength - 1 - i)
                                           | otherwise = go (i - 1) (ResolverState ss' ftype)
      go _ _ = pure ()
  go (length ss - 1) $ ResolverState ss ftype

resolve :: ResolverConstrains m => Expr -> Int -> m ()
resolve expr depth = tell $ Map.singleton expr depth

resolveStmts :: ResolverConstrains m => [Stmt] -> m ()
resolveStmts = void . traverse resolveStmt

beginScope :: (MonadState ResolverState m) => m ()
beginScope = get >>= \(ResolverState s ftype) -> put $ ResolverState ([Map.empty] <> s) ftype

endScope :: (MonadState ResolverState m) => m ()
endScope = get >>= \(ResolverState s ftype) -> put $ ResolverState (tail s) ftype

declare :: ResolverConstrains m => Token -> m ()
declare name = do
  (ResolverState ss ftype) <- get
  case ss of
    x : xs -> do
      when (Map.lookup (name ^. lexeme) x /= Nothing) $ do
        throwError $ ParserError name "Variable with this name already declared in this scope."
      put $ ResolverState ((Map.insert (name ^. lexeme) False x ) : xs) ftype
    _ -> pure ()

define :: (MonadState ResolverState m) => Token -> m ()
define name = do
  (ResolverState ss ftype) <- get
  case ss of
    x : xs -> put $ ResolverState ((Map.insert (name ^. lexeme) True x ) : xs) ftype
    _ -> pure ()
