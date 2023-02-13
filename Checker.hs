{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use <$>" #-}
module Checker (typed, showTypedExpr, collectTypeErrors) where

import Parser (Expr(..), Value(..))
import Data.Map as Map
import qualified Data.Maybe
import qualified Data.List
import qualified Data.Char

data MonoType = TConst String | TVar String | TFun MonoType MonoType | TApp String [MonoType] | Unknown
    deriving (Show, Eq)

boolType = TConst "Bool"
numType = TConst "Num"
strType = TConst "Str"

data PolyType =  TPoly [String] MonoType
    deriving (Show, Eq)

polyUnknown = TPoly [] Unknown

showTMono :: MonoType -> String
showTMono (TConst s) = s
showTMono (TVar s) = s
showTMono (TFun t1@(TFun _ _) t2) = "(" ++ showTMono t1 ++ ") -> " ++ showTMono t2
showTMono (TFun t1 t2) = showTMono t1 ++ " -> " ++ showTMono t2
showTMono (TApp s ms) = s ++ " " ++ (unwords $ fmap showTMonoE ms)
showTMono Unknown = "_"

showTMonoE t@(TConst _) = showTMono t
showTMonoE t@(TVar _) = showTMono t
showTMonoE t = "(" ++ showTMono t ++ ")"

showTPoly :: PolyType -> String
showTPoly (TPoly [] m) = showTMono m -- TODO dissallow polytypes without quantifiers. Non-empty list? -- type NonEmptyList a = (a, [a])
showTPoly (TPoly ss m) = unwords (fmap (\s -> "∀ " ++ s ++ " .") ss) ++ " " ++ showTMono m

showTPolyE t@(TPoly [] m) = showTMonoE m
showTPolyE t = "(" ++ showTPoly t ++ ")"

occursM :: String -> MonoType -> Bool
occursM s (TConst c) = False
occursM s (TVar c) = s == c
occursM s (TFun e1 e2) = occursM s e1 || occursM s e2
occursM s (TApp m ms) = any (occursM s) ms

-- occursP :: String -> PolyType -> Bool
-- occursP s (TPoly _ p) = _1 -- TODO occursP s p
--
-- normalizeQuantified :: PolyType -> PolyType
-- normalizeQuantified (TPoly s p) = if occursP s p then TPoly s (normalizeQuantified p) else normalizeQuantified p
-- normalizeQuantified e = e

data TypeErrorMessage = ApplicationMismatch PolyType PolyType | ArgumentMismatch PolyType PolyType | NoTopLevelBinding | UnexpectedParseError | UnboundVar Value | InfiniteType | DifferentTypeFunctions
    deriving Show

data TypedExpr = TypedVar Value PolyType | TypedApp [TypedExpr] PolyType | TypedAbs String TypedExpr PolyType | TypedLet String TypedExpr TypedExpr PolyType | TypedLetTop String TypedExpr PolyType | NoExpr | TypeError TypeErrorMessage
    deriving Show

showType = showTPoly
showTypeE = showTPolyE

showTypeError (ApplicationMismatch ft arg) = "PolyType error: tried to apply argument of type " ++ showTypeE arg ++ " to non function of type " ++ showTypeE ft ++ ""
showTypeError (ArgumentMismatch param arg) = "PolyType error: tried to apply argument of type " ++ showTypeE arg ++ " to function that takes a " ++ showTypeE param ++ ""
showTypeError NoTopLevelBinding = "PolyType error: Encountered something other than a binding at the top level"
showTypeError UnexpectedParseError = "Compiler error: Encountered parsing error in checker"
showTypeError (UnboundVar (Id s)) = "PolyType error: found unbound variable " ++ s
showTypeError InfiniteType = "PolyType error: found infinite type"
showTypeError DifferentTypeFunctions = "PolyType error: can not unify differently structured type functions"

showTypedExpr :: TypedExpr -> String
showTypedExpr (TypedAbs s e t) = "(λ " ++ s ++ " . " ++ showTypedExpr e ++ "):" ++ showTypeE t
showTypedExpr (TypedApp exprs t) = "(" ++ (unwords $ fmap showTypedExpr exprs) ++ "):" ++ showTypeE t
showTypedExpr (TypedVar (Bol True) t) = "true:" ++ showType t
showTypedExpr (TypedVar (Bol False) t) = "false:" ++ showType t
showTypedExpr (TypedVar (Num i) t) = show i ++ ":" ++ showType t
showTypedExpr (TypedVar (Str s) t) = "\"" ++ s ++ "\"" ++ ":" ++ showType t
showTypedExpr (TypedVar (Id s) t) = s ++ ":" ++ showTypeE t
showTypedExpr (TypedLet i e1 e2 t) = "(let " ++ i ++ " : " ++ showType (typeOf e1) ++ " = " ++ showTypedExpr e1 ++ " in " ++ showTypedExpr e2 ++ "):" ++ showTypeE t
showTypedExpr (TypedLetTop i e t) = i ++ " : " ++ showType t ++ " = " ++ showTypedExpr e
showTypedExpr NoExpr = "\n"
showTypedExpr (TypeError e) = showTypeError e

typeOf :: TypedExpr -> PolyType
typeOf (TypedVar _ t) = t
typeOf (TypedApp _ t) = t
typeOf (TypedAbs _ _ t) = t
typeOf (TypedLet _ _ _ t) = t
typeOf (TypedLetTop _ _ t) = t
typeOf NoExpr = TPoly [] Unknown
typeOf (TypeError _) = TPoly [] Unknown

type NewVar = Int
type NewInst = Int

type State = (NewVar, NewInst)

newVar :: State -> (MonoType, State)
newVar (nv, ni) = (TVar ("'" ++ [Data.Char.chr nv]), (nv + 1, ni))

newInst :: State -> (MonoType, State)
newInst (nv, ni) = (TVar ("'t" ++ show ni), (nv, ni + 1))

type Context = Map String PolyType

typeVar :: State -> Context -> Value -> (State, Maybe PolyType)
typeVar state c (Bol _) = (state, Just $ TPoly [] boolType)
typeVar state c (Num _) = (state, Just $ TPoly [] numType)
typeVar state c (Str _) = (state, Just $ TPoly [] strType)
typeVar state c (Id s) = let l = Map.lookup s c in case l of
    -- TODO make context contain Either PolyType MonoType instead
    Just (TPoly [] m) -> (state, l)
    Just (TPoly (x:xs) m) ->
        let (newType, state') = newInst state in
        let subbedType = substituteType newType x m in
        (state, Just (TPoly [] subbedType)) -- TODO replace all xs with newInst
    Nothing -> (state, Nothing)

type Substitution = [(String, String)]

-- inst :: PolyType -> MonoType

-- TODO should probably also take Substitution instead of just String
substituteType :: MonoType -> String -> MonoType -> MonoType
substituteType newType name oldType@(TVar s) = if s == name then newType else oldType
substituteType newType name oldType = oldType -- TODO replacements in TApp

substitute :: Substitution -> Context -> Context
substitute _ c = c -- TODO should this be able to detect errors?

-- This is gamma bar in the let rule (Γ)
generalize :: Context -> PolyType -> PolyType -- TODO ML should be Mono -> Poly probably?
generalize c tm = tm -- TODO if variables in tm are free (unbound) they should be quantified here

-- mgu
mostGeneralUnifier :: MonoType -> MonoType -> Either Substitution TypeErrorMessage
mostGeneralUnifier m1 m2 = Left [] -- TODO

-- This is a hack we should not deal with polymorphic types outside let bindings
toMono (TPoly [] m) = m
toPoly m = TPoly [] m

-- https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
-- TODO algorithmW should only produce mono types and only the let rule should produce one polytype internally
-- TODO Change Context to String -> Either PolyType MonoType and abolish toPoly and toMono
algorithmW :: State -> Context -> Expr -> (TypedExpr, PolyType, Substitution, State)
algorithmW state c (Var term _) =
    let (state', maybeType) = typeVar state c term in
    case maybeType of
        Just t -> (TypedVar term t, t, [], state')
        Nothing -> (TypeError (UnboundVar term), polyUnknown, [], state')
algorithmW state c (App [e1, e2] _) =
    let (typedE1, t1, s1, state') = algorithmW state c e1 in
    let (t', state'') = newVar state' in
    let sContext = substitute s1 c in
    let (typedE2, t2, s2, state''') = algorithmW state'' sContext e2 in
    let mguS3 = mostGeneralUnifier (toMono t1) (TFun (toMono t2) t') in
    case mguS3 of
        Right error -> (TypeError error, polyUnknown, [], state''')
        -- TODO perform s3 substitute on t'
        Left s3 -> (TypedApp [typedE1, typedE2] (toPoly t'), toPoly t', s1 ++ s2 ++ s3, state''')
algorithmW state c (App _ _) = error "we only support single application right now"
algorithmW state c (Abs name _ e) =
    let (t, state') = newVar state in
    let newContext = insert name (toPoly t) c in
    let (typedE, t', s, state'') = algorithmW state' newContext e in
    let fnType = toPoly (TFun t (toMono t')) in
    (TypedAbs name typedE fnType, fnType, s, state'')
algorithmW state c (Let name _ e1 e2) =
    let (typedE1, t1, s1, state') = algorithmW state c e1 in
    let sContext = substitute s1 c in
    let gt = generalize sContext t1 in
    let newContext = insert name gt sContext in
    let (typedE2, t2, s2, state'') = algorithmW state' newContext e2 in
    (TypedLet name typedE1 typedE2 t2, t2, s1 ++ s2, state'')
algorithmW state c (LetTop term _ e) =
    let (typedE, t, s, state') = algorithmW state c e in
    (TypedLetTop term typedE t, t, s, state')
algorithmW state c (Enclosed e _) = algorithmW state c e
-- TODO remove these
algorithmW state c NewLine = (NoExpr, polyUnknown, [], state)
algorithmW state c (ParseError m) = (TypeError UnexpectedParseError, polyUnknown, [], state)

typeAndAddToContext :: ([TypedExpr], State, Context) -> Expr -> ([TypedExpr], State, Context)
typeAndAddToContext (texprs, state, c) e =
    let (typedE, t, s, state') = algorithmW state c e in
    case typedE of
        (TypedLetTop name _ t) ->
            let newContext = insert name t c in
            (texprs ++ [typedE], state', newContext)
        NoExpr -> (texprs ++ [NoExpr], state', c)
        (TypeError UnexpectedParseError) -> (texprs ++ [NoExpr], state', c)
        _ -> (texprs ++ [TypeError NoTopLevelBinding], state', c)

typed :: [Expr] -> [TypedExpr]
typed exprs =
    let initialState = (Data.Char.ord 'a', 0) in
    let context = insert "print" (TPoly [] (TFun strType strType)) Map.empty in
    let (typedExprs, _, _) = Data.List.foldl typeAndAddToContext ([], initialState, context) exprs in typedExprs

collectTypeError :: [TypeErrorMessage] -> TypedExpr -> [TypeErrorMessage]
collectTypeError prev n@(TypedApp exprs _) = Data.List.foldl collectTypeError prev exprs
collectTypeError prev n@(TypedAbs _ e _) = collectTypeError prev e
collectTypeError prev n@(TypedLet _ e e2 t) = collectTypeError (collectTypeError prev e) e2
collectTypeError prev n@(TypedLetTop _ e _) = collectTypeError prev e
collectTypeError prev (TypeError m) = prev ++ [m]
collectTypeError prev e = prev

collectTypeErrors :: [TypedExpr] -> [String]
collectTypeErrors exprs = fmap showTypeError $ Data.List.foldl collectTypeError ([] :: [TypeErrorMessage]) exprs

