{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Interpreter where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Lens
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.Time.Clock.POSIX
import Data.IORef

import Types
import Resolver hiding (define)

import qualified Data.Text as T
import qualified Data.Map as Map

arity :: LoxFunction -> Int
arity (LoxFunction _ functionParams _ _) = length functionParams
arity (NativeFunction arity' _ _) = arity'

clock :: LoxFunction
clock = NativeFunction
        0
        (const $ NumValue . realToFrac <$> getPOSIXTime)
        "<native fn>"

class MonadEnvironment m where
  define      :: T.Text -> Code -> m ()
  assignValue :: Token -> Code -> m ()
  getEnv      :: m Environment
  setEnv      :: Environment -> m ()
  extendEnv   :: Map.Map T.Text Code -> m ()
  lookUpVariable :: Token -> Expr -> m Code
  assignVariable :: Expr -> Token -> Code -> m ()
  getAt       :: Int -> T.Text -> m Code
  assignAt    :: Int -> Token -> Code -> m ()

data InterpreterState = InterpreterState
  { intEnv    :: !Environment
  , intLocals :: !(Map.Map Expr Int)
  }

instance ( Monad m
         , MonadError ErrorType m
         , MonadIO m) => MonadEnvironment (StateT InterpreterState m) where
  define name code = gets intEnv >>= applyToEnv (Map.insert name code)
  assignValue token code = do
    let putVal (Environment valMap enc) = do
          valMap' <- liftIO $ readIORef valMap
          case Map.lookup (token ^. lexeme) valMap' of
            Just _ -> liftIO $ modifyIORef' valMap (Map.insert (token ^. lexeme) code)
            Nothing -> void $ putVal enc
        putVal (GlobalEnvironment valMap) = do
          valMap' <- liftIO $ readIORef valMap
          case Map.lookup (token ^. lexeme) valMap' of
            Just _ -> liftIO $ modifyIORef' valMap (Map.insert (token ^. lexeme) code)
            Nothing -> throwError $ RuntimeError token ("Undefined variable '" <> token ^. lexeme <> "'.")
    env <- gets intEnv
    putVal env
  getEnv = gets intEnv
  setEnv env = do
    InterpreterState _ locals <- get
    put $ InterpreterState env locals
  extendEnv valMap = do
    newMap <- liftIO (newIORef valMap)
    InterpreterState env locals <- get
    put $ InterpreterState (Environment newMap env) locals
  lookUpVariable name expr = do
    InterpreterState env locals <- get
    let distance = Map.lookup expr locals
    case distance of
      Just d -> getAt d (name ^. lexeme)
      Nothing -> do
        mcode <- liftToEnv (Map.lookup (name ^. lexeme)) (getGlobalEnv env)
        case mcode of
          Just code -> pure code
          Nothing -> throwError $ RuntimeError name ("Undefined variable '" <> name ^. lexeme <> "'.")
  getAt distance name = do
    env <- gets intEnv
    ansEnv <- (liftToEnv id (ancestor distance env))
    case (Map.lookup name) ansEnv of
      Just c -> pure c
      Nothing -> error "Shouldn't happen"--throwError $ RuntimeError name ("Undefined variable '" <> name ^. lexeme <> "'.")
  assignVariable expr name value = do
    InterpreterState env locals <- get
    let distance = Map.lookup expr locals
    case distance of
      Just d -> assignAt d name value
      Nothing -> applyToEnv (Map.insert (name ^. lexeme) value) (getGlobalEnv env)
  assignAt distance name value = do
    env <- gets intEnv
    let ansEnv = ancestor distance env
    applyToEnv (Map.insert (name ^. lexeme) value) ansEnv

ancestor :: Int -> Environment -> Environment
ancestor d env | d > 0     = ancestor (d - 1) (enclosingEnv env)
               | otherwise = env

getGlobalEnv :: Environment -> Environment
getGlobalEnv (Environment _ enc) = getGlobalEnv enc
getGlobalEnv e = e

isTruthy :: Code -> Bool
isTruthy VoidValue = False
isTruthy (BoolValue b) = b
isTruthy _ = True

numberOperandError :: (MonadError ErrorType m) => Token -> m Code
numberOperandError token = throwError $ RuntimeError token "Operands must be numbers"

call :: (MonadEnvironment m, MonadError ErrorType m, MonadIO m)
     => LoxFunction -> [Code] -> m Code
call (LoxFunction _ params body env) arguments = do
  let checkIfReturnValue (ReturnValue val) = pure val
      checkIfReturnValue e = throwError e
  let setGlobalEnv (Environment valMap enc) b = Environment valMap $ setGlobalEnv enc b
      setGlobalEnv (GlobalEnvironment _) b = b
  curEnv <- getEnv
  let baseEnv = getGlobalEnv curEnv
  newMap <- liftIO (newIORef Map.empty)
  setEnv (Environment newMap $ setGlobalEnv env baseEnv)
  forM_ (zip arguments params) $ \(arg, param) -> do
    define (param ^. lexeme) arg
  val <-
    (traverse execute body >> pure VoidValue) `catchError` checkIfReturnValue
  baseEnv' <- getGlobalEnv <$> getEnv
  setEnv $ setGlobalEnv curEnv baseEnv'
  pure val
call (NativeFunction _ body _ ) arguments = liftIO $ body arguments

evaluate :: (MonadEnvironment m, MonadError ErrorType m, MonadIO m) => Expr -> m Code
evaluate (Call callee paren arguments) = do
  callee' <- evaluate callee
  arguments' <- forM arguments evaluate
  case callee' of
    FunctionValue function -> do
      when (length arguments /= arity function) $
        throwError $ RuntimeError paren $ T.pack $
        "Expected " <> show (arity function) <> " arguments but got " <> show (length arguments) <> "."
      call function arguments'
    _ -> throwError $ RuntimeError paren "Can only call functions and classes."
evaluate (Lambda arguments body) = do
  env <- getEnv
  let function = LoxFunction Nothing arguments body env
  pure $ FunctionValue function
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
evaluate v@(Variable token) = lookUpVariable token v
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
  extendEnv Map.empty
  void $ traverse execute ss `catchError`
    \e -> getEnv >>= \(Environment _ enc) -> setEnv enc >> throwError e
  env' <- getEnv
  case env' of
    (Environment _ enc) -> setEnv enc
    e -> setEnv e
execute (Print expr) = do
  x <- evaluate expr
  liftIO $ print $ show x
execute (Expression expr) = void $ evaluate expr
execute (Function fname arguments body) = do
  env <- getEnv
  let function = LoxFunction (Just fname) arguments body env
  define (fname ^. lexeme) $ FunctionValue function
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
execute (Return _ value) = do
  value' <- maybe (pure VoidValue) evaluate value
  throwError $ ReturnValue value'
execute (ReplExpression expr) = execute $ Print expr

interpret :: (MonadError ErrorType m, MonadEnvironment m, MonadIO m) => [Stmt] -> m ()
interpret ss = void $ traverse execute ss
