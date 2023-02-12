{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use <$>" #-}
module Checker (typed, showTypedExpr, collectTypeErrors) where

import Parser (Expr(..), Value(..))
import Data.Map as Map
import qualified Data.Maybe
import qualified Data.List
import qualified Data.Char

data Type =  Free String | Quantified String | Fun Type Type | Unknown
    deriving (Show, Eq)

freeBool = Free "Bool"
freeNum = Free "Num"
freeStr = Free "Str"

data TypeErrorMessage = ApplicationMismatch Type Type | ArgumentMismatch Type Type | NoTopLevelBinding | UnexpectedParseError
    deriving Show

data TypedExpr = TypedVar Value Type | TypedApp [TypedExpr] Type | TypedAbs String TypedExpr Type | TypedLet String TypedExpr TypedExpr | TypedLetTop String TypedExpr Type | NoExpr | TypeError TypeErrorMessage
    deriving Show

showType :: Type -> String
showType (Free s) = s
showType (Quantified i) = "'" ++ show i
showType (Fun t1@(Fun _ _) t2) = "(" ++ showType t1 ++ ") -> " ++ showType t2
showType (Fun t1 t2) = showType t1 ++ " -> " ++ showType t2
showType Unknown = "_"

showTypeE t@(Fun _ _) = "(" ++ showType t ++ ")"
showTypeE t = showType t

showTypeError (ApplicationMismatch ft arg) = "Type error: tried to apply argument of type '" ++ showType arg ++ "' to non function of type '" ++ showType ft ++ "'"
showTypeError (ArgumentMismatch param arg) = "Type error: tried to apply argument of type '" ++ showType arg ++ "' to function that takes a '" ++ showType param ++ "'"
showTypeError NoTopLevelBinding = "Type error: Encountered something other than a binding at the top level"
showTypeError UnexpectedParseError = "Compiler error: Encountered parsing error in checker"

showTypedExpr :: TypedExpr -> String
showTypedExpr (TypedAbs s e Unknown) = "(λ " ++ s ++ " . " ++ showTypedExpr e ++ "):" ++ showTypeE Unknown
showTypedExpr (TypedAbs s e t@(Fun argType _)) = "(λ " ++ s ++ " : " ++ showType argType ++ " . " ++ showTypedExpr e ++ "):" ++ showTypeE t
showTypedExpr (TypedAbs s e _) = error "Abstraction should never be inferred to be other than Unknown or Function"
showTypedExpr (TypedApp exprs t) = "(" ++ (unwords $ fmap showTypedExpr exprs) ++ "):" ++ showTypeE t
showTypedExpr (TypedVar (Bol True) t) = "true:" ++ showType t
showTypedExpr (TypedVar (Bol False) t) = "false:" ++ showType t
showTypedExpr (TypedVar (Num i) t) = show i ++ ":" ++ showType t
showTypedExpr (TypedVar (Str s) t) = "\"" ++ s ++ "\"" ++ ":" ++ showType t
showTypedExpr (TypedVar (Id s) t@(Fun _ _)) = s ++ ":" ++ showTypeE t
showTypedExpr (TypedVar (Id s) t) = s ++ ":" ++ showType t
showTypedExpr (TypedLet i e1 e2) = "let " ++ i ++ " : " ++ showType (typeOf e1) ++ " = " ++ showTypedExpr e1 ++ " in " ++ showTypedExpr e2
showTypedExpr (TypedLetTop i e t) = i ++ " : " ++ showType t ++ " = " ++ showTypedExpr e
showTypedExpr NoExpr = "\n"
showTypedExpr (TypeError e) = showTypeError e

type Context = (Map String Type, Int)

newVar :: Context -> (Context, String)
newVar (m, i) = ((m, i + 1), [Data.Char.chr i])

typeOf :: TypedExpr -> Type
typeOf (TypedVar _ t) = t
typeOf (TypedApp _ t) = t
typeOf (TypedAbs _ _ t) = t
typeOf (TypedLet _ _ e) = typeOf e
typeOf (TypedLetTop _ _ t) = t
typeOf NoExpr = Unknown
typeOf (TypeError _) = Unknown

typeValue :: Context -> Value -> Type
typeValue _ (Bol _) = freeBool
typeValue _ (Num _) = freeNum
typeValue _ (Str _) = freeStr
typeValue (m, _) (Id s) = Data.Maybe.fromMaybe Unknown (Map.lookup s m)

-- https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
typeExpr :: Context -> Expr -> (Context, TypedExpr)
typeExpr c (Var value ta) = (c, TypedVar value (typeValue c value))
typeExpr c (App [e1, e2] ta) =
    let (c', typedE1) = typeExpr c e1 in
    let (c'', typedE2) = typeExpr c' e2 in
    case (typeOf typedE1, typeOf typedE2) of
        (Fun Unknown return, _) -> (c'', TypedApp [typedE1, typedE2] return)
        (Fun _ return, Unknown) -> (c'', TypedApp [typedE1, typedE2] return)
        (Fun param return, arg) -> if param == arg then (c'', TypedApp [typedE1, typedE2] return) else (c'', TypeError (ArgumentMismatch param arg))
        (Unknown, _) -> (c'', TypedApp [typedE1, typedE2] Unknown)
        (f, arg) -> (c'', TypeError (ApplicationMismatch f arg))
typeExpr c (App _ _) = error "we only support single application right now"
typeExpr c (Abs s ta e) = let (c', typedE) = typeExpr c e in (c', TypedAbs s typedE (Fun Unknown (typeOf typedE))) -- TODO argtype need to get from context of typedE and unify with type assert ta
typeExpr c (Enclosed e _) = typeExpr c e
typeExpr c (LetTop s _ e) = let (c', typedE) = typeExpr c e in (c', TypedLetTop s typedE (typeOf typedE))
typeExpr c (Let s _ e1 e2) =
    let ((m, i), typedE1) = typeExpr c e1 in
    let newContext = (insert s (typeOf typedE1) m, i) in
    let (c', typedE2) = typeExpr newContext e2 in
    (c', TypedLet s typedE1 typedE2)
-- TODO remove these
typeExpr c NewLine = (c, NoExpr)
typeExpr c (ParseError m) = (c, TypeError UnexpectedParseError)

typeAndAddToContext :: (Context, [TypedExpr]) -> Expr -> (Context, [TypedExpr])
typeAndAddToContext (c, texprs) e = let (c'@(m, i), typedE) = typeExpr c e in
    case typedE of
        (TypedLetTop s _ t) -> ((insert s t m, i), texprs ++ [typedE])
        NoExpr -> (c', texprs ++ [NoExpr])
        (TypeError UnexpectedParseError) -> (c', texprs ++ [NoExpr])
        _ -> (c', texprs ++ [TypeError NoTopLevelBinding])

typed exprs =
    let context = (insert "print" (Fun freeStr freeStr) Map.empty, Data.Char.ord 'a') in
    Data.List.foldl typeAndAddToContext (context, []) exprs

collectTypeError :: [TypeErrorMessage] -> TypedExpr -> [TypeErrorMessage]
collectTypeError prev n@(TypedApp exprs _) = Data.List.foldl collectTypeError prev exprs
collectTypeError prev n@(TypedAbs _ e _) = collectTypeError prev e
collectTypeError prev n@(TypedLet _ e e2) = collectTypeError (collectTypeError prev e) e2
collectTypeError prev n@(TypedLetTop _ e _) = collectTypeError prev e
collectTypeError prev (TypeError m) = prev ++ [m]
collectTypeError prev e = prev

collectTypeErrors :: [TypedExpr] -> [String]
collectTypeErrors exprs = fmap showTypeError $ Data.List.foldl collectTypeError ([] :: [TypeErrorMessage]) exprs

