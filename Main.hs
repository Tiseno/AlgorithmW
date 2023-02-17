import Lexer (tokenize)
import Parser (parseProgram, prettyExpr, debugExpr, Expr, maybeParse, collectParseErrors, hasParseError)
import Checker (typed, showTypedExpr, collectTypeErrors, substituteInContextT, applySub)
import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Set
import System.Exit (exitWith, ExitCode (ExitFailure))

parseInput :: String -> Maybe [Expr]
parseInput input = do
    tokens <- tokenize input
    maybeParse tokens

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
        case parseInput input of
            Just exprs -> do
                putStr $ concatMap prettyExpr exprs
            Nothing -> exitWith (ExitFailure 1)
    else if Data.Set.member "--debug" argSet then do
        if null args then do
            putStrLn "Expected input file name"
            exitWith (ExitFailure 3)
        else do
            input <- readFile (head args)
            let tokens = tokenize input
            case tokens of
                Nothing -> exitWith (ExitFailure 1)
                Just toks -> do
                    let ast = parseProgram toks
                    let (typedAst, s, c) = typed ast
                    putStrLn "Tokens:"
                    print tokens
                    putStrLn ""
                    putStrLn "Ast:"
                    print ast
                    putStrLn ""
                    putStrLn "File:"
                    putStrLn input
                    putStrLn "Parsed:"
                    putStrLn $ concatMap prettyExpr ast
                    -- putStrLn "Checked:"
                    -- putStrLn $ concatMap showTypedExpr typedAst
                    -- mapM_ putStrLn $ collectTypeErrors typedAst
                    -- putStrLn "Context:"
                    -- print $ c
                    -- putStrLn "Substitutions:"
                    -- print $ s
                    -- putStrLn "Context (sub):"
                    -- print $ substituteInContextT c s
                    putStrLn ""
                    putStrLn "Checked (sub):"
                    putStrLn $ concatMap showTypedExpr $ applySub typedAst s
                    mapM_ putStrLn $ collectTypeErrors typedAst


    -- Default is to tokenize, parse, and check from given filename argument
    else do
        input <- readFile (head args)
        case checkInput input of
            Just strings -> mapM_ putStrLn strings
            Nothing -> exitWith (ExitFailure 2)
