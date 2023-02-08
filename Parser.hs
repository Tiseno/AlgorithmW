{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Parser (Expr(..), Value(..), parse, prettyExpr, debugExpr) where

import Lexer (Token(..))
import Control.Applicative
import Data.Bifunctor (second)
import Data.List (intersperse)

data ErrorMessage = NoParse | UnclosedParenthesis | EndOfInput | ExpectedAssignment | UnexpectedToken Token | EOF
    deriving Show

data Value = Num Int | Str String | Id String
    deriving Show

type OfType = Maybe String

data Expr = Var Value OfType | App [Expr] OfType | Abs String OfType Expr | Let String OfType Expr | Enclosed Expr OfType | NewLine | Error ErrorMessage
    deriving Show

debugShowType (Just t) = " : " ++ t
debugShowType _ = " : _"

debugExpr :: Expr -> String
debugExpr (Var (Num i) t) = "Num [" ++ show i ++ debugShowType t ++ "]"
debugExpr (Var (Str s) t) = "Str [\"" ++ s ++ "\"" ++ debugShowType t ++ "]"
debugExpr (Var (Id s) t) = "Id [" ++ s ++ debugShowType t ++ "]"
debugExpr (App exprs t) = "App [(" ++ (unwords $ fmap debugExpr exprs) ++ ")" ++ debugShowType t ++ "]"
debugExpr (Abs s t e) = "Abs [" ++ s ++ debugShowType t ++ ", " ++ debugExpr e ++ "]"
debugExpr (Let i t e) = "Let [" ++ i ++ debugShowType t ++ "]" ++ " = [" ++ debugExpr e ++ "]"
debugExpr (Enclosed e t) = "Enclose [(" ++ debugExpr e ++ ")" ++ debugShowType t ++ "]"
debugExpr NewLine = "\n"
debugExpr (Error m) = "ERROR: " ++ show m

prettyShowType (Just t) = " : " ++ t
prettyShowType _ = ""

prettyExpr :: Expr -> String
prettyExpr (Abs s t e) = "λ " ++ s ++ prettyShowType t ++ " . " ++ prettyExpr e
prettyExpr (App exprs _) = unwords $ fmap prettyExpr exprs
prettyExpr (Var (Num i) t) = show i ++ prettyShowType t
prettyExpr (Var (Str s) t) = "\"" ++ s ++ "\"" ++ prettyShowType t
prettyExpr (Var (Id s) t) = s ++ prettyShowType t
prettyExpr (Enclosed e t) = "(" ++ prettyExpr e ++ ")" ++ prettyShowType t
prettyExpr (Let i t e) = i ++ prettyShowType t ++ " = " ++ prettyExpr e
prettyExpr NewLine = "\n"
prettyExpr (Error m) = "ERROR: " ++ show m

-- Grammar
-- te   := int | str
-- t    := (: te)?
-- e2   := (e1) | λid.e1 | x | num | "lit"
-- e1   := e2 e2*
-- a    := id t `=` e1
-- program := a*

parseSingleExpr :: [Token] -> ([Token], Expr)
parseSingleExpr tokens@(TLambda : xs) = case xs of
    (TIdentifier i) : TAbstraction : xs' -> let (rest, expr) = parseExpr1 xs' in (rest, Abs i Nothing expr)
    _ -> (tokens, Error NoParse)
parseSingleExpr tokens@(TLParen : xs) =
    let (rest, result) = parseExpr1 xs in case rest of
    TRParen : xs -> (xs, Enclosed result Nothing)
    _ -> (tokens, Error UnclosedParenthesis)
parseSingleExpr ((TIdentifier i) : xs) = (xs, Var (Id i) Nothing)
parseSingleExpr ((TNumberLiteral n) : xs) = (xs, Var (Num n) Nothing)
parseSingleExpr ((TStringLiteral s) : xs) = (xs, Var (Str s) Nothing)
parseSingleExpr tokens = (tokens, Error EndOfInput)

parseExpr1' :: [Expr] -> [Token] -> ([Token], [Expr])
parseExpr1' prev tokens = let (rest, result) = parseSingleExpr tokens in case result of
    (Error _) -> (tokens, prev)
    _ -> parseExpr1' (prev ++ [result]) rest

parseExpr1 (TNewLine : xs) = parseExpr1 xs
parseExpr1 tokens = let (tokens', exprs) = parseExpr1' [] tokens in
    if length exprs == 1 then (tokens', head exprs) else (tokens', App exprs Nothing)

parseAssign (TNewLine : xs) = (xs, NewLine)
parseAssign ((TIdentifier i) : xs) = case xs of
    (TAssign : xs') -> let (rest, result) = parseExpr1 xs' in (rest, Let i Nothing result)
    _ -> (xs, Error ExpectedAssignment)
parseAssign tokens@(x:xs) = (tokens, Error (UnexpectedToken x))
parseAssign [] = ([], Error EOF)

parseProgram' :: [Expr] -> [Token] -> ([Token], [Expr])
parseProgram' prev tokens = let (rest, result) = parseAssign tokens in
    case result of
        (Error _) -> (tokens, prev)
        _ -> parseProgram' (prev ++ [result]) rest

parseProgram :: [Token] -> [Expr]
parseProgram tokens = snd $ parseProgram' [] tokens

parse Nothing  = [Error NoParse]
parse (Just tokens) = parseProgram tokens

