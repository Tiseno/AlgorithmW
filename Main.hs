import Lexer (tokenize)
import Parser (parse, prettyExpr, debugExpr)
import Checker (typed, showTypedExpr)
import System.Environment (getArgs)
import Data.List (intercalate)

main = do
    args <- getArgs
    file <- readFile (head args)
    let tokens = tokenize file
    print tokens
    let ast = parse tokens
    putStrLn ""
    print ast
    putStrLn ""
    putStr $ concatMap debugExpr ast
    putStrLn ""
    putStrLn ""
    putStr $ concatMap prettyExpr ast
    let typedAst = typed ast
    print typedAst
    putStrLn ""
    putStr $ concatMap showTypedExpr typedAst
