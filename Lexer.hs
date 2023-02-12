{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use <$" #-}
{-# HLINT ignore "Use const" #-}
module Lexer (Token(..), tokenize) where

import Data.Bifunctor (second)
import Data.Char (isAlpha, isNumber, isSpace)
import Data.Tuple (swap)
import Text.Read (readMaybe)
import Control.Applicative

data Token = TNewLine
    | TLambda         -- λ
    | TAbstraction    -- .
    | TLParen         -- (
    | TRParen         -- )
    | TAssign         -- =
    | TOfType         -- :
    | TLet            -- let
    | TIn             -- in
    | TBoolLiteral Bool
    | TNumberLiteral Int
    | TStringLiteral String
    | TIdentifier String
    -- | TComment String -- -- a comment
    -- | TTypeIdentifier String
    deriving Show

newtype Lexer a = Lexer { runLexer :: String -> Maybe (String, a) }

instance Functor Lexer where
    fmap f (Lexer l) = Lexer $ \input -> fmap (second f) (l input)

instance Applicative Lexer where
    pure a = Lexer $ \input -> Just (input, a)
    (Lexer l1) <*> (Lexer l2) = Lexer $ \input -> do
        (input', f) <- l1 input
        fmap (second f) (l2 input')

instance Alternative Lexer where
    empty = Lexer $ \_ -> Nothing
    (Lexer l1) <|> (Lexer l2) = Lexer $ \input -> l1 input <|> l2 input

charLexer :: Char -> Lexer Char
charLexer c = Lexer f
    where
        f (x:xs)
            | x == c = Just (xs, c)
            | otherwise = Nothing
        f [] = Nothing

lambda      = const TLambda      <$> charLexer 'λ'
abstraction = const TAbstraction <$> charLexer '.'
lParen      = const TLParen      <$> charLexer '('
rParen      = const TRParen      <$> charLexer ')'
assign      = const TAssign      <$> charLexer '='
ofType      = const TOfType      <$> charLexer ':'

notEmpty :: Lexer String -> Lexer String
notEmpty (Lexer l) = Lexer $ \input -> do
    (input', result) <- l input
    if null result then Nothing else Just (input', result)

spanLexer :: (Char -> Bool) -> Lexer String
spanLexer predicate = Lexer $ \input -> return $ swap $ span predicate input

whitespace = spanLexer (\c -> c == '\t' || c == '\r' || c == '\f' || c == '\v' || c == ' ')

newLine = const TNewLine <$> charLexer '\n'

identifier = TIdentifier <$> notEmpty (spanLexer isAlpha)

numberLiteral = TNumberLiteral <$> Lexer (\input -> do
    (input', result) <- runLexer (notEmpty (spanLexer isNumber)) input
    return (input', read result :: Int))

stringLiteral :: Lexer Token
stringLiteral = TStringLiteral <$> (charLexer '"' *> spanLexer (/= '"') <* charLexer '"')

specialIdentifier = TIdentifier <$> notEmpty (spanLexer (\c -> not (isSpace c) && not (isAlpha c) && not (isNumber c) && c /= 'λ'  && c /= '.' && c /= '(' && c /= ')' && c /= '=' && c /= ':' && c /= '"'))

keywordLexer = traverse charLexer

keywordLet = const TLet <$> keywordLexer "let"
keywordIn  = const TIn  <$> keywordLexer "in"

boolLiteral = tokenConstructor <$> (keywordLexer "true" <|> keywordLexer "false")
    where
        tokenConstructor "true" = TBoolLiteral True
        tokenConstructor "false" = TBoolLiteral False

testTokenLexer name (lexer :: Lexer Token) = do
    putStr (name ++ " token > ")
    input <- getLine
    print $ runLexer lexer input

testHelpLexer name lexer = do
    putStr (name ++ " help > ")
    input <- getLine
    print $ runLexer lexer input

testWithInput name fn = do
    putStr (name ++ " > ")
    input <- getLine
    print $ fn input

sepBy separatorLexer itemLexer = many (separatorLexer *> itemLexer)

tokenize :: String -> Maybe [Token]
tokenize s = snd <$> runLexer (sepBy whitespace (
    lambda
    <|> abstraction
    <|> lParen
    <|> rParen
    <|> assign
    <|> newLine
    <|> ofType
    <|> keywordLet
    <|> keywordIn
    <|> boolLiteral
    <|> numberLiteral
    <|> stringLiteral
    <|> identifier
    <|> specialIdentifier
    )) s


main = do
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testWithInput "tokenizer" tokenize
    testTokenLexer "specialIdentifier"      specialIdentifier
    testTokenLexer "specialIdentifier"      specialIdentifier
    testTokenLexer "specialIdentifier"      specialIdentifier
    testTokenLexer "specialIdentifier"      specialIdentifier
    testTokenLexer "specialIdentifier"      specialIdentifier
    testTokenLexer "specialIdentifier"      specialIdentifier
    testTokenLexer "stringLiteral"      stringLiteral
    testTokenLexer "lambda"      lambda
    testHelpLexer "whitespace"   whitespace
    testTokenLexer "abstraction" abstraction
    testTokenLexer "lParen"      lParen
    testTokenLexer "rParen"      rParen
    testTokenLexer "assign"      assign
    testTokenLexer "identifier"  identifier
    testTokenLexer "numberLiteral"      numberLiteral

