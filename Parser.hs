{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Parser (Expr(..), Value(..), parseProgram, prettyExpr, debugExpr, maybeParse, collectParseErrors, hasParseError) where

import Lexer (Token(..))
import qualified Data.List

data ParseErrorMessage = MalformedAbstration | MalformedLetBinding | UnclosedParenthesis | EndOfInput | ExpectedAssignment | UnexpectedToken Token
    deriving Show

showParseError MalformedAbstration = "Parse error: malformed abstraction"
showParseError MalformedLetBinding = "Parse error: malformed let binding"
showParseError UnclosedParenthesis = "Parse error: missing closing parenthesis"
showParseError EndOfInput = "Parse error: reached end of input"
showParseError ExpectedAssignment = "Parse error: expected assignment"
showParseError (UnexpectedToken t) = "Parse error: unexpected token " ++ show t

data Value = Num Int | Str String | Id String
    deriving Show

type OfType = Maybe String

data Expr = Var Value OfType | App [Expr] OfType | Abs String OfType Expr | Let String OfType Expr Expr | LetTop String OfType Expr | Enclosed Expr OfType | NewLine | ParseError ParseErrorMessage | EOF
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
debugExpr (ParseError m) = "ERROR: " ++ show m

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
prettyExpr (ParseError m) = "ERROR: " ++ show m

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
    _ -> (tokens, ParseError MalformedAbstration)
parseSingleExpr tokens@(TLParen : xs) =
    let (rest, result) = parseExpr1 xs in case rest of
    TRParen : xs -> (xs, Enclosed result Nothing)
    _ -> (tokens, ParseError UnclosedParenthesis)
parseSingleExpr tokens@(TLet : xs) = case xs of
    (TIdentifier i) : TAssign : xs' -> let (rest, expr) = parseExpr1 xs' in case rest of
        (TIn : xs'') -> let (rest', expr2) = parseExpr1 xs'' in (rest', Let i Nothing expr expr2)
        _ -> (tokens, ParseError MalformedLetBinding)
    _ -> (tokens, ParseError MalformedLetBinding)
parseSingleExpr ((TIdentifier i) : xs) = (xs, Var (Id i) Nothing)
parseSingleExpr ((TNumberLiteral n) : xs) = (xs, Var (Num n) Nothing)
parseSingleExpr ((TStringLiteral s) : xs) = (xs, Var (Str s) Nothing)
parseSingleExpr tokens = (tokens, ParseError EndOfInput)

parseExpr1' :: [Expr] -> [Token] -> ([Token], [Expr])
parseExpr1' prev tokens = let (rest, result) = parseSingleExpr tokens in case result of
    (ParseError _) -> (tokens, prev)
    _ -> parseExpr1' (prev ++ [result]) rest

parseExpr1 (TNewLine : xs) = parseExpr1 xs
parseExpr1 tokens = let (tokens', exprs) = parseExpr1' [] tokens in
    if length exprs == 1 then (tokens', head exprs) else (tokens', App exprs Nothing)

parseAssign (TNewLine : xs) = (xs, NewLine)
parseAssign ((TIdentifier i) : xs) = case xs of
    (TAssign : xs') -> let (rest, result) = parseExpr1 xs' in (rest, LetTop i Nothing result)
    _ -> (xs, ParseError ExpectedAssignment)
parseAssign tokens@(x:xs) = (tokens, ParseError (UnexpectedToken x))
parseAssign [] = ([], EOF)

parseProgram' :: [Expr] -> [Token] -> ([Token], [Expr])
parseProgram' prev tokens = let (rest, result) = parseAssign tokens in
    case result of
        EOF -> (tokens, prev)
        (ParseError _) -> (tokens, prev ++ [result])
        _ -> parseProgram' (prev ++ [result]) rest

parseProgram :: [Token] -> [Expr]
parseProgram tokens = snd $ parseProgram' [] tokens

hasParseError :: Expr -> Bool
hasParseError (ParseError _) = True
hasParseError (App exprs _) = any hasParseError exprs
hasParseError (Abs _ _ e) = hasParseError e
hasParseError (LetTop _ _ e) = hasParseError e
hasParseError (Let _ _ e1 e2) = hasParseError e1 || hasParseError e2
hasParseError (Enclosed e _) = hasParseError e
hasParseError _ = False

maybeParse tokens = let parsed = parseProgram tokens in
    if any hasParseError parsed then Nothing else Just parsed

collectParseError :: [ParseErrorMessage] -> Expr -> [ParseErrorMessage]
collectParseError prev n@(App exprs _) = Data.List.foldl collectParseError prev exprs
collectParseError prev n@(Abs _ _ e) = collectParseError prev e
collectParseError prev n@(Let _ _ e e2) = collectParseError (collectParseError prev e) e2
collectParseError prev n@(LetTop _ _ e) = collectParseError prev e
collectParseError prev (ParseError m) = prev ++ [m]
collectParseError prev e = prev

collectParseErrors :: [Expr] -> [String]
collectParseErrors exprs = fmap showParseError $ Data.List.foldl collectParseError [] exprs
