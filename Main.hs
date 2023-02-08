import Lexer (tokenize)
import Parser (parse, prettyExpr, debugExpr)
import Checker (typed, showTypedExpr, collectErrors)
import System.Environment (getArgs)
import Data.List (intercalate)

main = do
    args <- getArgs
    file <- readFile (head args)
    let tokens = tokenize file
    let ast = parse tokens
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
