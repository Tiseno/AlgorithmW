{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Parser (Expr(..), Value(..), parseProgram, prettyExpr, debugExpr, maybeParse) where

import Lexer (Token(..))

data ErrorMessage = MalformedAbstration | MalformedLetBinding | UnclosedParenthesis | EndOfInput | ExpectedAssignment | UnexpectedToken Token
    deriving Show

data Value = Num Int | Str String | Id String
    deriving Show

type OfType = Maybe String

data Expr = Var Value OfType | App [Expr] OfType | Abs String OfType Expr | Let String OfType Expr Expr | LetTop String OfType Expr | Enclosed Expr OfType | NewLine | Error ErrorMessage | EOF
    deriving Show

debugShowType (Just t) = " : " ++ t
debugShowType _ = " : _"

debugExpr :: Expr -> String
debugExpr (Var (Num i) t) = "Num [" ++ show i ++ debugShowType t ++ "]"
debugExpr (Var (Str s) t) = "Str [\"" ++ s ++ "\"" ++ debugShowType t ++ "]"
debugExpr (Var (Id s) t) = "Id [" ++ s ++ debugShowType t ++ "]"
debugExpr (App exprs t) = "App [(" ++ (unwords $ fmap debugExpr exprs) ++ ")" ++ debugShowType t ++ "]"
debugExpr (Abs s t e) = "Abs [" ++ s ++ debugShowType t ++ ", " ++ debugExpr e ++ "]"
debugExpr (LetTop i t e) = "LetTop [" ++ i ++ debugShowType t ++ "]" ++ " = [" ++ debugExpr e ++ "]"
debugExpr (Let i t e1 e2) = "Let [" ++ i ++ debugShowType t ++ "]" ++ " = [" ++ debugExpr e1 ++ "] in [" ++ debugExpr e2 ++ "]"
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
prettyExpr (LetTop i t e) = i ++ prettyShowType t ++ " = " ++ prettyExpr e
prettyExpr (Let i t e1 e2) = "let " ++ i ++ prettyShowType t ++ " = " ++ prettyExpr e1 ++ " in " ++ prettyExpr e2
prettyExpr NewLine = "\n"
prettyExpr (Error m) = "ERROR: " ++ show m

-- Grammar
-- te   := int | str
-- t    := (: te)?
-- e2   := (e1) | λid.e1 | x | num | "lit" | let x = e1 in e1
-- e1   := e2 e2*
-- a    := id t `=` e1
-- program := a*

parseSingleExpr :: [Token] -> ([Token], Expr)
parseSingleExpr tokens@(TLambda : xs) = case xs of
    (TIdentifier i) : TAbstraction : xs' -> let (rest, expr) = parseExpr1 xs' in (rest, Abs i Nothing expr)
    _ -> (tokens, Error MalformedAbstration)
parseSingleExpr tokens@(TLParen : xs) =
    let (rest, result) = parseExpr1 xs in case rest of
    TRParen : xs -> (xs, Enclosed result Nothing)
    _ -> (tokens, Error UnclosedParenthesis)
parseSingleExpr tokens@(TLet : xs) = case xs of
    (TIdentifier i) : TAssign : xs' -> let (rest, expr) = parseExpr1 xs' in case rest of
        (TIn : xs'') -> let (rest', expr2) = parseExpr1 xs'' in (rest', Let i Nothing expr expr2)
        _ -> (tokens, Error MalformedLetBinding)
    _ -> (tokens, Error MalformedLetBinding)
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
    (TAssign : xs') -> let (rest, result) = parseExpr1 xs' in (rest, LetTop i Nothing result)
    _ -> (xs, Error ExpectedAssignment)
parseAssign tokens@(x:xs) = (tokens, Error (UnexpectedToken x))
parseAssign [] = ([], EOF)

parseProgram' :: [Expr] -> [Token] -> ([Token], [Expr])
parseProgram' prev tokens = let (rest, result) = parseAssign tokens in
    case result of
        EOF -> (tokens, prev)
        (Error _) -> (tokens, prev ++ [result])
        _ -> parseProgram' (prev ++ [result]) rest

parseProgram :: [Token] -> [Expr]
parseProgram tokens = snd $ parseProgram' [] tokens

hasError :: Expr -> Bool
hasError (Error _) = True
hasError (App exprs _) = any hasError exprs
hasError (Abs _ _ e) = hasError e
hasError (LetTop _ _ e) = hasError e
hasError (Let _ _ e1 e2) = hasError e1 || hasError e2
hasError (Enclosed e _) = hasError e
hasError _ = False

maybeParse tokens = let parsed = parseProgram tokens in
    if any hasError parsed then Nothing else Just parsed

