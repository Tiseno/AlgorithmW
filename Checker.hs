{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use <$>" #-}
module Checker (typed, showTypedExpr, collectTypeErrors) where

import Parser (Expr(..), Value(..))
import Data.Map as Map
import qualified Data.Maybe
import qualified Data.List
import qualified Data.Char

data TMono = TConst String | TVar String | TFun TMono TMono | TApp String [TMono] | Unknown
    deriving (Show, Eq)

createFnType :: TMono -> Type -> Type
createFnType m t@(TQuantifier s p)  = TQuantifier s (createFnType m p)
createFnType m t@(TPMono m2) = TPMono $ TFun m m2

boolType = TConst "Bool"
numType = TConst "Num"
strType = TConst "Str"

data Type =  TQuantifier String Type | TPMono TMono
    deriving (Show, Eq)

showTMono :: TMono -> String
showTMono (TConst s) = s
showTMono (TVar s) = s
showTMono (TFun t1@(TFun _ _) t2) = "(" ++ showTMono t1 ++ ") -> " ++ showTMono t2
showTMono (TFun t1 t2) = showTMono t1 ++ " -> " ++ showTMono t2
showTMono (TApp s ms) = s ++ " " ++ (unwords $ fmap showTMonoE ms)
showTMono Unknown = "_"

showTMonoE t@(TConst _) = showTMono t
showTMonoE t@(TVar _) = showTMono t
showTMonoE t = "(" ++ showTMono t ++ ")"

showTPoly :: Type -> String
showTPoly (TQuantifier s p) = "∀ " ++ s ++ " . " ++ showTPoly p
showTPoly (TPMono m) = showTMono m

showTPolyE t@(TPMono (TConst _)) = showTPoly t
showTPolyE t@(TPMono (TVar _)) = showTPoly t
showTPolyE t = "(" ++ showTPoly t ++ ")"

occursM :: String -> TMono -> Bool
occursM s (TConst c) = False
occursM s (TVar c) = s == c
occursM s (TFun e1 e2) = occursM s e1 || occursM s e2
occursM s (TApp m ms) = any (occursM s) ms

occursP :: String -> Type -> Bool
occursP s (TQuantifier _ p) = occursP s p
occursP s (TPMono m) = occursM s m

normalizeQuantified :: Type -> Type
normalizeQuantified (TQuantifier s p) = if occursP s p then TQuantifier s (normalizeQuantified p) else normalizeQuantified p
normalizeQuantified e = e

data TypeErrorMessage = ApplicationMismatch Type Type | ArgumentMismatch Type Type | NoTopLevelBinding | UnexpectedParseError
    deriving Show

data TypedExpr = TypedVar Value Type | TypedApp [TypedExpr] Type | TypedAbs String TypedExpr Type | TypedLet String TypedExpr TypedExpr | TypedLetTop String TypedExpr Type | NoExpr | TypeError TypeErrorMessage
    deriving Show

showType = showTPoly
showTypeE = showTPolyE

showTypeError (ApplicationMismatch ft arg) = "Type error: tried to apply argument of type " ++ showTypeE arg ++ " to non function of type " ++ showTypeE ft ++ ""
showTypeError (ArgumentMismatch param arg) = "Type error: tried to apply argument of type " ++ showTypeE arg ++ " to function that takes a " ++ showTypeE param ++ ""
showTypeError NoTopLevelBinding = "Type error: Encountered something other than a binding at the top level"
showTypeError UnexpectedParseError = "Compiler error: Encountered parsing error in checker"

showTypedExpr :: TypedExpr -> String
showTypedExpr (TypedAbs s e t) = "(λ " ++ s ++ " . " ++ showTypedExpr e ++ "):" ++ showTypeE t
showTypedExpr (TypedApp exprs t) = "(" ++ (unwords $ fmap showTypedExpr exprs) ++ "):" ++ showTypeE t
showTypedExpr (TypedVar (Bol True) t) = "true:" ++ showType t
showTypedExpr (TypedVar (Bol False) t) = "false:" ++ showType t
showTypedExpr (TypedVar (Num i) t) = show i ++ ":" ++ showType t
showTypedExpr (TypedVar (Str s) t) = "\"" ++ s ++ "\"" ++ ":" ++ showType t
showTypedExpr (TypedVar (Id s) t) = s ++ ":" ++ showType t
showTypedExpr (TypedLet i e1 e2) = "(let " ++ i ++ " : " ++ showType (typeOf e1) ++ " = " ++ showTypedExpr e1 ++ " in " ++ showTypedExpr e2 ++ "):" ++ showType (typeOf e2)
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
typeOf NoExpr = TPMono Unknown
typeOf (TypeError _) = TPMono Unknown

typeVar :: Context -> Value -> (Context, Type)
typeVar c (Bol _) = (c, TPMono boolType)
typeVar c (Num _) = (c, TPMono numType)
typeVar c (Str _) = (c, TPMono strType)
typeVar c@(m, _) (Id s) = case Map.lookup s m of
    Just some -> (c, some)
    Nothing -> (c, TPMono Unknown) -- let (c', s) = newVar c in (c', Quantified s) -- NOTE inst should be used here, newVar belongs in App and Abs

-- https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
typeExpr :: Context -> Expr -> (Context, TypedExpr)
typeExpr c (Var value ta) = let (c', t) = typeVar c value in (c', TypedVar value t)
typeExpr c (App [e1, e2] ta) =
    let (c', typedE1) = typeExpr c e1 in
    let (c'', typedE2) = typeExpr c' e2 in
    case (typeOf typedE1, typeOf typedE2) of
        (TPMono (TFun Unknown return), _) -> (c'', TypedApp [typedE1, typedE2] (TPMono return))
        (TPMono (TFun _ return), TPMono Unknown) -> (c'', TypedApp [typedE1, typedE2] (TPMono return))
        (TPMono (TFun param return), TPMono arg) -> if param == arg then (c'', TypedApp [typedE1, typedE2] (TPMono return)) else (c'', TypeError (ArgumentMismatch (TPMono param) (TPMono arg)))
        -- TODO handle when typedE2 is polytype, what should happen?
        (TPMono Unknown, _) -> (c'', TypedApp [typedE1, typedE2] (TPMono Unknown))
        (f, arg) -> (c'', TypeError (ApplicationMismatch f arg))
typeExpr c (App _ _) = error "we only support single application right now"
typeExpr c (Abs s ta e) =
    let (c', v) = newVar c in
    let (c'', typedE) = typeExpr c e in
    (c', TypedAbs s typedE (createFnType Unknown (typeOf typedE))) -- TODO argtype need to get from context of typedE and unify with type assert ta
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
    let context = (insert "print" (TPMono (TFun strType strType)) Map.empty, Data.Char.ord 'a') in
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

