{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Checker (typed, showTypedExpr) where

import Parser (Expr(..), Value(..))
import Data.Map as Map
import qualified Data.Maybe

data Type =  Mono String | Fun Type Type | Unknown
    deriving (Show, Eq)

data TypedExpr = TypedVar Value Type | TypedApp [TypedExpr] Type | TypedAbs String TypedExpr Type | TypedLet String TypedExpr Type | NoExpr | ErrorT
    deriving Show

showType :: Type -> String
showType (Mono s) = s
showType (Fun t1@(Fun _ _) t2) = "(" ++ showType t1 ++ ") -> " ++ showType t2
showType (Fun t1 t2) = showType t1 ++ " -> " ++ showType t2
showType Unknown = "_"

showTypedExpr :: TypedExpr -> String
showTypedExpr (TypedAbs s e Unknown) = "(λ " ++ s ++ " . " ++ showTypedExpr e ++ "):(" ++ showType Unknown ++ ")"
showTypedExpr (TypedAbs s e t@(Fun argType _)) = "(λ " ++ s ++ " : " ++ showType argType ++ " . " ++ showTypedExpr e ++ "):(" ++ showType t ++ ")"
showTypedExpr (TypedApp exprs t) = "(" ++ (unwords $ fmap showTypedExpr exprs) ++ "):(" ++ showType t ++ ")"
showTypedExpr (TypedVar (Num i) t) = show i ++ ":" ++ showType t
showTypedExpr (TypedVar (Str s) t) = "\"" ++ s ++ "\"" ++ ":" ++ showType t
showTypedExpr (TypedVar (Id s) t@(Fun _ _)) = s ++ ":(" ++ showType t ++ ")"
showTypedExpr (TypedVar (Id s) t) = s ++ ":" ++ showType t
showTypedExpr (TypedLet i e t) = i ++ " : " ++ showType t ++ " = " ++ showTypedExpr e
showTypedExpr NoExpr = "\n"
showTypedExpr ErrorT = "ERROR"

type Context = Map String Type

typeOf :: TypedExpr -> Type
typeOf (TypedVar _ t) = t
typeOf (TypedApp _ t) = t
typeOf (TypedAbs _ _ t) = t
typeOf (TypedLet _ _ t) = t
typeOf NoExpr = Unknown
typeOf ErrorT = Unknown

typeValue :: Context -> Value -> Type
typeValue _ (Num _) = Mono "num"
typeValue _ (Str _) = Mono "str"
typeValue c (Id s) = Data.Maybe.fromMaybe Unknown (Map.lookup s c)

-- syntax directed https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Syntax-directed_rule_system
typeExpr :: Context -> Expr -> TypedExpr
typeExpr c (Var value ta) = TypedVar value (typeValue c value)
typeExpr c (App [e1, e2] ta) =
    let typedE1 = typeExpr c e1
        typedE2 = typeExpr c e2 in
        case typeOf typedE1 of
            f@(Fun param return) -> case typeOf typedE2 of
                Unknown -> TypedApp [typedE1, typedE2] Unknown -- TODO infer
                arg -> if param == arg then TypedApp [typedE1, typedE2] return else error "Type error: mismatch in application"
            Unknown -> TypedApp [typedE1, typedE2] Unknown -- TODO infer
            _ -> error "Type error: non function type in the lhs of application"
typeExpr c (App _ ta) = undefined
typeExpr c (Abs s ta e) = let typedE = typeExpr c e in TypedAbs s typedE (Fun Unknown (typeOf typedE)) -- TODO argtype need to get from context of typedE and unify with type assert ta
typeExpr c (Enclosed e t) =  typeExpr c e
typeExpr c (Let s t e) = let typedE = typeExpr c e in TypedLet s typedE (typeOf typedE)
-- TODO remove these
typeExpr c NewLine = NoExpr
typeExpr c (Error m) = ErrorT

typed :: [Expr] -> [TypedExpr]
typed = let context = insert "print" (Fun (Mono "str") (Mono "str")) Map.empty in
    fmap (typeExpr context)



