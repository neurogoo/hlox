{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Main where

import System.Environment
import System.Exit
import System.IO
import Data.Text.IO (readFile)
import Control.Monad (forever, forM_)
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Lens
import Data.Maybe (catMaybes)
import Debug.Trace
import Text.Read (readMaybe)

import Types

import qualified Data.Text as T
import qualified Data.Map as Map

error :: Int -> T.Text -> IO ()
error line message = report line "" message

report :: Int -> T.Text -> T.Text -> IO ()
report line where_ message =
  putStrLn $ T.unpack $ "[line " <> T.pack (show line) <> "] Error" <> where_ <> ": " <> message

data ScanState = ScanState
  { _start   :: !Int
  , _current :: !Int
  , _line    :: !Int
  , _source  :: !T.Text
  } deriving (Show)

makeLenses ''ScanState

addSingleToken :: TokenType -> Char -> Int -> Maybe Token
addSingleToken tt c line = Just $ Token tt (T.singleton c) Nothing line

addToken :: TokenType -> T.Text -> Int -> Maybe Token
addToken tt s line = Just $ Token tt s Nothing line

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlpha c || isDigit c

stringToNumericLiteral :: String -> Maybe LiteralType
stringToNumericLiteral t = NumericLiteral <$> (readMaybe t :: Maybe Double)

process :: (MonadState ScanState m) => (Char -> Bool) -> String -> String -> m (String, String)
process comp (c : xss) s
  | comp c = do
      source %= T.drop 1
      process comp xss (s <> [c])
  | otherwise = pure (s, ([c] <> xss))
process comp "" s = pure (s, "")

scanToken :: (MonadState ScanState m, MonadWriter [(Int, String)] m) => m (Maybe Token)
scanToken = do
  current += 1
  line' <- use line
  s <- use source
  source %= T.drop 1
  case T.unpack s of
    '(' : xs -> pure $ addSingleToken LeftParen '(' line'
    ')' : xs -> pure $ addSingleToken RightParen ')' line'
    '{' : xs -> pure $ addSingleToken LeftBrace '{' line'
    '}' : xs -> pure $ addSingleToken RightBrace '}' line'
    ',' : xs -> pure $ addSingleToken Comma ',' line'
    '.' : xs -> pure $ addSingleToken Dot '.' line'
    '-' : xs -> pure $ addSingleToken Minus '-' line'
    '+' : xs -> pure $ addSingleToken Plus '+' line'
    ';' : xs -> pure $ addSingleToken Semicolon ';' line'
    '*' : xs -> pure $ addSingleToken Star '*' line'
    '!' : xs -> case xs of
      '=' : xss -> do
        source %= T.drop 1
        pure $ addToken BangEqual "!=" line'
      _ -> pure $ addSingleToken Bang '!' line'
    '=' : xs -> case xs of
      '=' : xss -> do
        source %= T.drop 1
        pure $ addToken EqualEqual "==" line'
      _ -> pure $ addSingleToken Equal '=' line'
    '<' : xs -> case xs of
      '=' : xss -> do
        source %= T.drop 1
        pure $ addToken LessEqual "<=" line'
      _ -> pure $ addSingleToken Less '<' line'
    '>' : xs -> case xs of
      '=' : xss -> do
        source %= T.drop 1
        pure $ addToken GreaterEqual ">=" line'
      _ -> pure $ addSingleToken Greater '>' line'
    '/' : xs -> case xs of
      '/' : xss -> do
        source %= T.drop 1
        let ignoreComment ('\n' : xsss) = pure Nothing
            ignoreComment (_ : xsss) = do
                source %= T.drop 1
                ignoreComment xsss
            ignoreComment _ = pure Nothing
        ignoreComment xss
      _ -> pure $ addSingleToken Slash '/' line'
    ' ' : _ -> pure Nothing
    '\r' : _ -> pure Nothing
    '\t' : _ -> pure Nothing
    '\n' : _ -> do
      line += 1
      pure Nothing
    '"' : xs -> do
      let processString ('"' : xss) s = do
            source %= T.drop 1
            pure $ Just s
          processString ("") s = pure Nothing
          processString (c : xss) s = do
            when (c == '\n') $ line += 1
            source %= T.drop 1
            processString xss (s <> [c])
      res <- processString xs ""
      case res of
        Just s -> do
          currentLine <- use line
          pure $ Just $ Token String (T.pack s) Nothing currentLine
        Nothing -> do
          currentLine <- use line
          tell [(currentLine, "Unterminated string.")]
          pure Nothing
    x : xs
      | isDigit x -> do
      (n1,xss') <- process isDigit xs [x]
      res <-
        case xss' of
          '.' : c : xss
            | isDigit c -> do
                source %= T.drop 1
                Just <$> process isDigit ([c] <> xss) "."
            | otherwise -> pure Nothing
          _ -> pure Nothing
      case res of
        Just (n2, _) -> pure $ Just $ Token Number (T.pack $ n1 <> n2) (stringToNumericLiteral $ n1 <> n2) line'
        Nothing -> pure $ Just $ Token Number (T.pack n1) (stringToNumericLiteral n1) line'
      | isAlphaNumeric x -> do
          (identifier, _) <- process isAlphaNumeric xs [x]
          case Map.lookup (T.pack identifier) keywords of
            Just v -> pure $ Just $ Token v (T.pack identifier) Nothing line'
            Nothing -> pure $ Just $ Token Identifier (T.pack identifier) Nothing line'
    _ -> do
      tell [(line', "Unexpected character.")]
      pure Nothing

scanTokens :: T.Text -> Writer [(Int, T.Text)] [Token]
scanTokens source = go [] initialState []
  where
    go tokens (ScanState _ _ line' "") errors = do
      tell $ fmap (\(a,b) -> (a, T.pack b)) errors
      pure $ catMaybes tokens <> [endToken line']
    go tokens state errors = do
      let ((token, state'), errors') =
            runIdentity $ runWriterT $ flip runStateT state $ scanToken
      go (tokens <> [token]) state' (errors <> errors')
    initialState = ScanState
      { _start = 0
      , _current = 0
      , _line = 1
      , _source = source
      }
    endToken = Token Eof "" Nothing


runFile :: String -> IO ()
runFile filename = do
  fileData <- Data.Text.IO.readFile filename
  hadError <- run fileData
  if hadError then exitWith (ExitFailure 65) else pure ()

run :: T.Text -> IO Bool
run source = do
  let (tokens, errors) = runWriter $ scanTokens source
  forM_ errors $ uncurry Main.error
  forM_ tokens $ \token -> do
    print token
  pure $ null errors

runPrompt :: IO ()
runPrompt = forever $ do
  putStr "> "
  hFlush stdout
  userInput <- getLine
  run $ T.pack userInput

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile filename
    (_ : _) -> do
      putStrLn "Usage, hlox [script]"
      exitWith (ExitFailure 64)
    _ -> runPrompt
