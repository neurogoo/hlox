{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
module Parser where

import Control.Lens
import Data.Maybe
import Control.Monad.State.Strict
import Debug.Trace
import Control.Monad.Except

import Types

import qualified Data.Text as T

data Parser = Parser
  { _tokens     :: ![Token]
  , _prev       :: !(Maybe Token)
  , _errors     :: ![(Token,T.Text)]
  , _parserMode :: !CompilerMode
  }

makeLenses ''Parser

class MonadParser m where
  match         :: TokenType -> m (Either Token ())
  peek          :: m Token
  advance       :: m Token
  parseError    :: (Token, T.Text) -> m ()
  getParserMode :: m CompilerMode

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
  getParserMode = use parserMode

type ParserConstrains m = (Monad m, MonadParser m, MonadError (Token, T.Text) m)

matchWith :: ParserConstrains m => TokenType -> (Token -> m a) -> m (Maybe a)
matchWith tt stmt = do
  x <- match tt
  case x of
    Left t -> do
      s <- stmt t
      pure $ Just s
    Right _ -> pure Nothing

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
  expr <- or'
  m <- match Equal
  case m of
    Left equals -> do
      value <- assignment
      case expr of
        Variable name -> pure $ Assign name value
        _ -> throwParseError (equals, "Invalid assignment target.") --TODO: just report error, not throw
    Right () -> pure expr

or' :: ParserConstrains m => m Expr
or' = do
  let go expr = do
        x <- match Or
        case x of
          Left operator -> do
            right <- and'
            go $ Logical expr operator right
          Right _ -> pure expr
  expr <- and'
  go expr

and' :: ParserConstrains m => m Expr
and' = do
  let go expr = do
        x <- match And
        case x of
          Left operator -> do
            right <- equality
            go $ Logical expr operator right
          Right _ -> pure expr
  expr <- equality
  go expr

declaration :: ParserConstrains m => m [Stmt]
declaration = sequence [go] `catchError` const (synchronize >> pure [])
  where
    go = do
      m <- myAsum [ matchWith VAR $ const varDeclaration
                  , matchWith FUN $ const $ function FunctionType
                  ]
      maybe statement pure m

functionBody :: ParserConstrains m => FunctionType -> m ([Token], [Stmt])
functionBody kind = do
  void $ consume LeftParen $ T.pack $ "Expect '(' after " <> show kind <> " name."
  let go parameters = do
        when (length parameters >= 255) $
          peek >>= \p -> throwParseError (p, "Cannot have more than 255 parameters.")
        identifier <- consume IDENTIFIER "Expect parameter name."
        x <- match Comma
        case x of
          Left _ -> go (parameters <> [identifier])
          Right () -> pure $ parameters <> [identifier]
  x <- check RightParen
  parameters <-
    if not x then
      go []
    else
      pure []
  void $ consume RightParen "Expect ')' after parameters."
  void $ consume LeftBrace $ T.pack $ "Expect '{' before " <> show kind <> " body."
  body <- block
  pure (parameters, body)

function :: ParserConstrains m => FunctionType -> m Stmt
function kind = do
  name <- consume IDENTIFIER $ T.pack $ "Expect " <> show kind <> " name."
  (parameters, body) <- functionBody kind
  pure $ Function name parameters body

lambda :: ParserConstrains m => m Expr
lambda = do
  (parameters, body) <- functionBody FunctionType
  pure $ Lambda parameters body

varDeclaration :: ParserConstrains m => m Stmt
varDeclaration = do
  name <- consume IDENTIFIER "Expect variable name."
  m <- match Equal
  initializer <-
    case m of
      Left _ -> Just <$> expression
      Right () -> pure $ Nothing
  void $ consume Semicolon "Expect ';' after variable declaration."
  pure $ Var name initializer

myAsum :: ParserConstrains m => [m (Maybe a)] -> m (Maybe a)
myAsum = go
  where
    go (x : xs) = do
      x' <- x
      case x' of
        Just x'' -> pure $ Just x''
        Nothing -> go xs
    go [] = pure Nothing

statement :: ParserConstrains m => m Stmt
statement = do
  x <- myAsum [ matchWith For $ const forStatement
              , matchWith IF $ const ifStatement
              , matchWith PRINT $ const printStatement
              , matchWith RETURN $ returnStatement
              , matchWith WHILE $ const whileStatement
              , matchWith LeftBrace $ const (Block <$> block) ]
  maybe expressionStatement pure x

returnStatement :: ParserConstrains m => Token -> m Stmt
returnStatement keyword = do
  value <- do
    c <- check Semicolon
    if not c then
      Just <$> expression
    else
      pure Nothing
  void $ consume Semicolon "Expect ';' after return value."
  pure $ Return keyword value

block :: ParserConstrains m => m [Stmt]
block = do
  let getStatements ss = do
        a <- check RightBrace
        b <- isAtEnd
        if not a && not b then do
          s <- declaration
          getStatements (ss <> s)
        else
          pure ss
  ss <- getStatements []
  void $ consume RightBrace "Expect '}' after block."
  pure ss

forStatement :: ParserConstrains m => m Stmt
forStatement = do
  void $ consume LeftParen "Expect '(' after 'for'."
  x <- match Semicolon
  initializer <-
    case x of
      Left _ -> pure $ Nothing
      Right _ -> do
        x' <- match VAR
        case x' of
          Left _ -> Just <$> varDeclaration
          Right _ -> Just <$> expressionStatement
  x' <- check Semicolon
  condition <-
    if not x' then
      Just <$> expression
    else
      pure Nothing
  void $ consume Semicolon "Expect ';' after loop condition."

  x'' <- check RightParen
  increment <-
    if not x'' then
      Just <$> expression
    else
      pure Nothing
  void $ consume RightParen "Expect ')' after for clauses."
  body <- statement
  body' <-
    case increment of
      Just i -> pure $ Block [body, Expression i]
      Nothing -> pure $ body
  body'' <-
    case condition of
      Just c -> pure $ While c body'
      Nothing -> pure $ While (Literal $ BooleanLiteral True) body'
  case initializer of
    Just i -> pure $ Block [i, body'']
    Nothing -> pure body''


whileStatement :: ParserConstrains m => m Stmt
whileStatement = do
  void $ consume LeftParen "Expect '(' after 'while'."
  condition <- expression
  void $ consume RightParen "Expect ')' after condition."
  body <- statement
  pure $ While condition body

ifStatement :: ParserConstrains m => m Stmt
ifStatement = do
  void $ consume LeftParen "Expect '(' after 'if'"
  condition <- expression
  void $ consume RightParen "Expect ')' after if condition"
  thenBranch <- statement
  x <- match Else
  case x of
    Left _ -> do
      elseBranch <- statement
      pure $ If condition thenBranch $ Just elseBranch
    _ -> pure $ If condition thenBranch Nothing

printStatement :: ParserConstrains m => m Stmt
printStatement = do
  value <- expression
  void $ consume Semicolon "Expect ';' after value."
  pure $ Print value

expressionStatement :: ParserConstrains m => m Stmt
expressionStatement = do
  value <- expression `catchError` (error . show)
  mode <- getParserMode
  case mode of
    Compile -> do
      void $ consume Semicolon "Expect ';' after expression."
      pure $ Expression value
    Repl -> do
      (consume Semicolon "Expect ';' after expression." >> pure (Expression value))
      `catchError`
        (\e -> do
            p <- peek
            if p ^. tokenType == Eof then
              pure $ ReplExpression value
            else
              throwError e)


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
    Right () -> call

call :: ParserConstrains m => m Expr
call = do
  let go expr = do
        x <- match LeftParen
        case x of
          Left _ -> do
            finishCall expr >>= go
          _ -> pure expr
  expr <- primary
  go expr

finishCall :: ParserConstrains m => Expr -> m Expr
finishCall callee = do
  let go arguments = do
        e <- expression
        x <- match Comma
        case x of
          Left _ -> do
            when (length arguments >= 255) $ do
              p <- peek
              parseError (p, "Cannot have more than 255 arguments.")
            go (arguments <> [e])
          _ -> pure (arguments <> [e])
  x <- check RightParen
  arguments <-
    if not x then
      go []
    else
      pure []
  paren <- consume RightParen "Expect ')' after arguments."
  pure $ Call callee paren arguments

primary :: ParserConstrains m => m Expr
primary = do
  let func = do
        expr <- expression
        void $ consume RightParen "Expect ')' after expression."
        pure $ Grouping expr
  x <- myAsum [ matchWith FALSE (const $ pure $ Literal $ BooleanLiteral False)
              , matchWith TRUE (const $ pure $ Literal $ BooleanLiteral True)
              , matchWith Nil (const $ pure $ Literal $ EmptyLiteral)
              , matchWith Number (\(Token _ _ (Just l) _ ) -> pure $ Literal l)
              , matchWith String (\(Token _ _ (Just l) _ ) -> pure $ Literal l)
              , matchWith IDENTIFIER (pure . Variable)
              , matchWith LeftParen (const $ func)
              , matchWith FUN (const $ lambda)
              ]
  case x of
    Just expr -> pure expr
    Nothing -> do
      p <- peek
      traceShow (p ^. tokenLine) $ throwParseError (p, "Couldn't find matching primary for token")

parse :: CompilerMode -> [Token] -> Either [(Token, T.Text)] [Stmt]
parse mode ts = either (Left . pure) processNonCatchedErrors returnValue
  where
    processNonCatchedErrors (ss, (Parser _ _ [] _)) = Right ss
    processNonCatchedErrors (_, (Parser _ _ err _)) = Left err
    returnValue = flip runStateT (Parser ts Nothing [] mode) $ program []
    program ss = do
      p <- peek
      case p of
        (Token Eof _ _ _) -> pure ss
        _ -> do
          s <- declaration
          program (ss <> s)

--TODO: add problem error reporting for this
synchronize :: (Monad m, MonadParser m, MonadError (Token, T.Text) m) => m ()
synchronize = do
    previous <- advance
    go $ previous ^. tokenType
  where
    go previous = do
      case previous of
        Semicolon -> pure ()
        Eof -> pure ()
        _ -> do
          current <- peek
          case current ^. tokenType of
            CLASS -> pure ()
            FUN -> pure ()
            VAR -> pure ()
            For -> pure ()
            IF -> pure ()
            WHILE -> pure ()
            PRINT -> pure ()
            RETURN -> pure ()
            _ -> do
              t'' <- advance
              go $ t'' ^. tokenType
