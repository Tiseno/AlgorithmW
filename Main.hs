{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Lexer (tokenize)
import Parser (parseProgram, prettyExpr, debugExpr, Expr, maybeParse, collectParseErrors, hasParseError)
import Checker (typed, showTypedExpr, collectTypeErrors, substituteInContextT, applySub, showTPoly, showTMono, showTContext)
import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Set
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Map (assocs)

parseInput :: String -> Maybe [Expr]
parseInput input = do
    tokens <- tokenize input
    maybeParse tokens

maybeFormat :: String -> Maybe String
maybeFormat input = do
    tokens <- tokenize input
    exprs <- maybeParse tokens
    pure $ concatMap prettyExpr exprs

checkInput :: String -> Maybe [String]
checkInput input = do
    tokens <- tokenize input
    let ast = parseProgram tokens
    if any hasParseError ast then
        return $ collectParseErrors ast
    else
        let (typedAst, _, _) = typed ast in
        return $ collectTypeErrors typedAst

main = do
    args <- getArgs
    let argSet = Data.Set.fromList args
    if Data.Set.member "--stdin-format" argSet then do
        input <- getContents
        case maybeFormat input of
            Just result -> putStr result
            Nothing -> exitWith (ExitFailure 1)
    else if Data.Set.member "--debug" argSet then do
        input <- readFile (head args)
        case tokenize input of
            Nothing -> exitWith (ExitFailure 1)
            Just toks -> do
                let ast = parseProgram toks
                let (typedAst, s, c) = typed ast
                putStrLn "File:"
                putStrLn input
                putStrLn "Parsed and formatted:"
                putStrLn $ concatMap prettyExpr ast
                putStrLn "Substitutions:"
                mapM_ putStrLn $ fmap (\(t1, t2) -> showTMono t1 ++ " →  " ++ showTMono t2) s
                putStrLn ""
                putStrLn "Context:"
                let sContextList = assocs $ substituteInContextT c s
                mapM_ putStrLn $ fmap (\(name, p) -> name ++ " = " ++ showTContext p) $ sContextList
                putStrLn ""
                putStrLn "Checked:"
                putStrLn $ concatMap showTypedExpr $ applySub typedAst s
                mapM_ putStrLn $ collectTypeErrors typedAst

    -- Default is to tokenize, parse, and check from given filename argument
    else do
        input <- readFile (head args)
        case checkInput input of
            Just strings -> mapM_ putStrLn strings
            Nothing -> exitWith (ExitFailure 2)
