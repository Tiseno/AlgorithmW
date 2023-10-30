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

data Value = Bol Bool | Num Int | Str String | Id String
    deriving Show

data Expr = Var Value | App [Expr] | Abs String Expr | Let String Expr Expr | LetTop String Expr | Enclosed Expr | NewLine | ParseError ParseErrorMessage | EOF
    deriving Show

debugExpr :: Expr -> String
debugExpr (Var (Bol b)) = "Bool [" ++ show b ++ "]"
debugExpr (Var (Num i)) = "Num [" ++ show i ++ "]"
debugExpr (Var (Str s)) = "Str [\"" ++ s ++ "\"]"
debugExpr (Var (Id s)) = "Id [" ++ s ++ "]"
debugExpr (App exprs) = "App [" ++ (unwords $ fmap debugExpr exprs) ++ "]"
debugExpr (Abs s e) = "Abs [" ++ s ++ ", " ++ debugExpr e ++ "]"
debugExpr (LetTop i e) = "LetTop [" ++ i ++ "]" ++ " = [" ++ debugExpr e ++ "]"
debugExpr (Let i e1 e2) = "Let [" ++ i ++ "]" ++ " = [" ++ debugExpr e1 ++ "] in [" ++ debugExpr e2 ++ "]"
debugExpr (Enclosed e) = "Enclose [(" ++ debugExpr e ++ ")" ++ "]"
debugExpr NewLine = "\n"
debugExpr (ParseError m) = "ERROR: " ++ show m

prettyShowType (Just t) = " : " ++ t
prettyShowType _ = ""

prettyExpr :: Expr -> String
prettyExpr (Abs s e) = "λ " ++ s ++ " . " ++ prettyExpr e
prettyExpr (App exprs) = unwords $ fmap prettyExpr exprs
prettyExpr (Var (Bol True)) = "true"
prettyExpr (Var (Bol False)) = "false"
prettyExpr (Var (Num i)) = show i
prettyExpr (Var (Str s)) = "\"" ++ s ++ "\""
prettyExpr (Var (Id s)) = s
prettyExpr (Enclosed e) = "(" ++ prettyExpr e ++ ")"
prettyExpr (LetTop i e) = i ++ " = " ++ prettyExpr e
prettyExpr (Let i e1 e2) = "let " ++ i ++ " = " ++ prettyExpr e1 ++ " in " ++ prettyExpr e2
prettyExpr NewLine = "\n"
prettyExpr (ParseError m) = "ERROR: " ++ show m

parseSingleExpr :: [Token] -> ([Token], Expr)
parseSingleExpr tokens@(TLambda : xs) = case xs of
    (TIdentifier i) : TAbstraction : xs' -> let (rest, expr) = parseExpr1 xs' in (rest, Abs i expr)
    _ -> (tokens, ParseError MalformedAbstration)
parseSingleExpr tokens@(TLParen : xs) =
    let (rest, result) = parseExpr1 xs in case rest of
    TRParen : xs -> (xs, Enclosed result)
    _ -> (tokens, ParseError UnclosedParenthesis)
parseSingleExpr tokens@(TLet : xs) = case xs of
    (TIdentifier i) : TAssign : xs' -> let (rest, expr) = parseExpr1 xs' in case rest of
        (TIn : xs'') -> let (rest', expr2) = parseExpr1 xs'' in (rest', Let i expr expr2)
        _ -> (tokens, ParseError MalformedLetBinding)
    _ -> (tokens, ParseError MalformedLetBinding)
parseSingleExpr ((TIdentifier i) : xs) = (xs, Var (Id i))
parseSingleExpr ((TNumberLiteral n) : xs) = (xs, Var (Num n))
parseSingleExpr ((TStringLiteral s) : xs) = (xs, Var (Str s))
parseSingleExpr ((TBoolLiteral b) : xs) = (xs, Var (Bol b))
parseSingleExpr tokens = (tokens, ParseError EndOfInput)

parseExpr1' :: [Expr] -> [Token] -> ([Token], [Expr])
parseExpr1' prev tokens = let (rest, result) = parseSingleExpr tokens in case result of
    (ParseError _) -> (tokens, prev)
    _ -> parseExpr1' (prev ++ [result]) rest

parseExpr1 (TNewLine : xs) = parseExpr1 xs
parseExpr1 tokens = let (tokens', exprs) = parseExpr1' [] tokens in
    if length exprs == 1 then (tokens', head exprs) else (tokens', App exprs)

parseAssign (TNewLine : xs) = (xs, NewLine)
parseAssign ((TIdentifier i) : xs) = case xs of
    (TAssign : xs') -> let (rest, result) = parseExpr1 xs' in (rest, LetTop i result)
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
hasParseError (App exprs) = Prelude.any hasParseError exprs
hasParseError (Abs _ e) = hasParseError e
hasParseError (LetTop _ e) = hasParseError e
hasParseError (Let _ e1 e2) = hasParseError e1 || hasParseError e2
hasParseError (Enclosed e) = hasParseError e
hasParseError _ = False

maybeParse tokens = let parsed = parseProgram tokens
    in if any hasParseError parsed then Nothing else Just parsed

collectParseError :: [ParseErrorMessage] -> Expr -> [ParseErrorMessage]
collectParseError prev n@(App exprs) = Data.List.foldl collectParseError prev exprs
collectParseError prev n@(Abs _ e) = collectParseError prev e
collectParseError prev n@(Let _ e e2) = collectParseError (collectParseError prev e) e2
collectParseError prev n@(LetTop _ e) = collectParseError prev e
collectParseError prev (ParseError m) = prev ++ [m]
collectParseError prev e = prev

collectParseErrors :: [Expr] -> [String]
collectParseErrors exprs = fmap showParseError $ foldl collectParseError [] exprs
