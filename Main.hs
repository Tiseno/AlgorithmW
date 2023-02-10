import Lexer (tokenize)
import Parser (parseProgram, prettyExpr, debugExpr, Expr, maybeParse)
import Checker (typed, showTypedExpr, collectErrors)
import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Set
import System.Exit (exitWith, ExitCode (ExitFailure))

maybeParseInput :: String -> Maybe [Expr]
maybeParseInput input = do
    tokens <- tokenize input
    maybeParse tokens

main = do
    args <- getArgs
    let argSet = Data.Set.fromList args
    if Data.Set.member "--stdin-format" argSet then do
        input <- getContents
        case maybeParseInput input of
            Just exprs -> do
                putStr $ concatMap prettyExpr exprs
            Nothing -> exitWith (ExitFailure 1)
    else do
        if null args then do
            putStrLn "Expected input file name"
            exitWith (ExitFailure 1)
        else do
            file <- readFile (head args)
            let tokens = tokenize file
            case tokens of
                Nothing -> exitWith (ExitFailure 1)
                Just toks -> do
                    let ast = parseProgram toks
                    let typedAst = snd $ typed ast
                    putStrLn "Tokens:"
                    print tokens
                    putStrLn ""
                    putStrLn "Ast:"
                    print ast
                    putStrLn ""
                    putStrLn "File:"
                    putStrLn file
                    putStrLn "Parsed:"
                    putStrLn $ concatMap prettyExpr ast
                    putStrLn "Checked:"
                    putStrLn $ concatMap showTypedExpr typedAst
                    mapM_ putStrLn $ collectErrors typedAst
