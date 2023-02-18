{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
module Checker (typed, showTypedExpr, collectTypeErrors, substituteInContextT, applySub, showTPoly, showTMono) where

import Parser (Expr(..), Value(..), prettyExpr)
import Data.Map as Map hiding (foldl)
import qualified Data.Maybe
import qualified Data.List
import qualified Data.Char
import Data.Bifunctor (first)
import Data.Set (Set, empty, singleton, union, toList)

data MonoType = TConst String | TVar String | TApp String [MonoType] | Unknown
    deriving (Show, Eq)

boolType = TConst "Bool"
numType = TConst "Num"
strType = TConst "Str"

data PolyType =  TPoly [String] MonoType
    deriving (Show, Eq)

polyUnknown = TPoly [] Unknown

showTMono :: MonoType -> String
showTMono (TVar s) = s
showTMono (TConst s) = s
showTMono (TApp "->" [t1@(TApp "->" _), t2]) = "(" ++ showTMono t1 ++ ") -> " ++ showTMono t2
showTMono (TApp "->" [t1, t2]) = showTMono t1 ++ " -> " ++ showTMono t2
showTMono (TApp "->" ts) = error "Type application for function type with " ++ show (length ts) ++ " arguments"
showTMono (TApp s ms) = s ++ " " ++ (unwords $ fmap showTMonoE ms)
showTMono Unknown = "_"

showTMonoE t@(TApp _ _) = "(" ++ showTMono t ++ ")"
showTMonoE t = showTMono t

showTPoly :: PolyType -> String
showTPoly (TPoly [] m) = showTMono m
showTPoly (TPoly ss m) = unwords (fmap (\s -> "∀ " ++ s ++ " .") ss) ++ " " ++ showTMono m

showTPolyE t@(TPoly [] m) = showTMonoE m
showTPolyE t = "(" ++ showTPoly t ++ ")"

occursM :: String -> MonoType -> Bool
occursM s (TVar c) = s == c
occursM s (TConst c) = s == c
occursM s (TApp "->" [e1, e2]) = occursM s e1 || occursM s e2
occursM s (TApp m ms) = any (occursM s) ms

data TypeErrorMessage = NoTopLevelBinding | UnexpectedParseError | UnboundVar Value | MguInfiniteType MonoType MonoType Expr | MguDifferentTypeFunctions MonoType MonoType Expr | MguMismatch MonoType MonoType Expr
    deriving Show

data TypedExpr = TypedVar Value PolyType | TypedApp [TypedExpr] PolyType | TypedAbs String MonoType TypedExpr PolyType | TypedLet String PolyType TypedExpr TypedExpr PolyType | TypedLetTop String TypedExpr PolyType | NoExpr | TypeError TypeErrorMessage
    deriving Show

showType = showTPoly
showTypeE = showTPolyE

showTypeError NoTopLevelBinding = "Compiler error: Encountered something other than a binding at the top level"
showTypeError UnexpectedParseError = "Compiler error: Encountered parsing error in checker"
showTypeError (UnboundVar v) = "Type error: found unbound variable " ++ show v
showTypeError (MguInfiniteType a b e) = "Type error: found infinite type in unification of " ++ showTMonoE a ++ " and " ++ showTMonoE b ++ " in the expression " ++ prettyExpr e
showTypeError (MguDifferentTypeFunctions a b e) = "Type error: can not unify differently structured type functions " ++ showTMonoE a ++ " and " ++ showTMonoE b ++ " in the expression " ++ prettyExpr e
showTypeError (MguMismatch a b e) = "Type error: can not unify " ++ showTMonoE a ++ " and " ++ showTMonoE b ++ " in the expression " ++ prettyExpr e

showTypedExpr :: TypedExpr -> String
showTypedExpr (TypedAbs s m e t) = "(λ " ++ s ++ " : " ++ showTMono m ++ " . " ++ showTypedExpr e ++ "):" ++ showTypeE t
showTypedExpr (TypedApp exprs t) = "(" ++ (unwords $ fmap showTypedExpr exprs) ++ "):" ++ showTypeE t
showTypedExpr (TypedVar (Bol True) t) = "true:" ++ showType t
showTypedExpr (TypedVar (Bol False) t) = "false:" ++ showType t
showTypedExpr (TypedVar (Num i) t) = show i ++ ":" ++ showType t
showTypedExpr (TypedVar (Str s) t) = "\"" ++ s ++ "\"" ++ ":" ++ showType t
showTypedExpr (TypedVar (Id s) t) = s ++ ":" ++ showTypeE t
showTypedExpr (TypedLet i p e1 e2 t) = "(let " ++ i ++ " : " ++ showType p ++ " = " ++ showTypedExpr e1 ++ " in " ++ showTypedExpr e2 ++ "):" ++ showTypeE t
showTypedExpr (TypedLetTop i e t) = i ++ " : " ++ showType t ++ " = " ++ showTypedExpr e
showTypedExpr NoExpr = "\n"
showTypedExpr (TypeError e) = showTypeError e

typeOf :: TypedExpr -> PolyType
typeOf (TypedVar _ t) = t
typeOf (TypedApp _ t) = t
typeOf (TypedAbs _ _ _ t) = t
typeOf (TypedLet _ _ _ _ t) = t
typeOf (TypedLetTop _ _ t) = t
typeOf NoExpr = TPoly [] Unknown
typeOf (TypeError _) = TPoly [] Unknown

type NewVar = Int
type NewInst = Int
type State = (NewVar, NewInst)

type Context = Map String PolyType

newVarName :: State -> (String, State)
newVarName (nv, ni) = ("'" ++ [Data.Char.chr nv], (nv + 1, ni))

newVar :: State -> (MonoType, State)
newVar s = first TVar $ newVarName s

newInstName :: State -> (String, State)
newInstName (nv, ni) = ("'t" ++ show ni, (nv, ni + 1))

newInst :: State -> (MonoType, State)
newInst s = first TVar $ newInstName s

typeVar :: State -> Context -> Value -> (State, Maybe PolyType)
typeVar state c (Bol _) = (state, Just $ TPoly [] boolType)
typeVar state c (Num _) = (state, Just $ TPoly [] numType)
typeVar state c (Str _) = (state, Just $ TPoly [] strType)
typeVar state c (Id s) = let l = Map.lookup s c in case l of
    -- TODO make context contain Either PolyType MonoType instead, this is just confusing
    Nothing -> (state, Nothing) -- TODO This should probably return error
    Just (TPoly [] m) -> (state, l)
    Just p -> let (state', mt) = inst state p in (state', Just $ TPoly [] mt)

inst :: State -> PolyType -> (State, MonoType)
inst state (TPoly quantifiers m) = let (state', subs) = Data.List.foldl createSubs (state, []) quantifiers in (state', subMultipleMonoTypeS m subs)
    where
        createSubs :: (State, [(String, String)]) -> String -> (State, [(String, String)])
        createSubs (state, prev) s = let (new, state') = newInstName state in (state', prev ++ [(s, new)])


type Substitution a = [(a, a)]
type TypeSubstitution = Substitution MonoType

substituteInContextT :: Context -> TypeSubstitution -> Context
substituteInContextT c subs = Data.List.foldl sub1 c subs
    where
        sub1 :: Context -> (MonoType, MonoType) -> Context
        sub1 c mapping = Map.map (`subPolyTypeT` mapping) c


subMultiplePolyTypeT :: PolyType -> [(MonoType, MonoType)] -> PolyType
subMultiplePolyTypeT m mappings = Data.List.foldl subPolyTypeT m mappings

subPolyTypeT :: PolyType -> (MonoType, MonoType) -> PolyType
subPolyTypeT (TPoly quantifiers mt) mapping = TPoly quantifiers (subMonoTypeT mt mapping) -- Should this substitute in qualifiers? Probably but how? That is only possible if 'to' is a variable and not a compound type

subMonoTypeT :: MonoType -> (MonoType, MonoType) -> MonoType
subMonoTypeT (TApp n ts) mapping = TApp n (fmap (`subMonoTypeT` mapping) ts)
subMonoTypeT current (from, to) = if current == from then to else current

subMultipleMonoTypeT :: MonoType -> [(MonoType, MonoType)] -> MonoType
subMultipleMonoTypeT m mappings = Data.List.foldl subMonoTypeT m mappings

subMatchingString :: String -> (String, String) -> String
subMatchingString current (from, to) = if current == from then to else current

subMultipleMonoTypeS :: MonoType -> [(String, String)] -> MonoType
subMultipleMonoTypeS m mappings = Data.List.foldl subMonoTypeS m mappings

subMonoTypeS :: MonoType -> (String, String) -> MonoType
subMonoTypeS (TVar current) mapping = TVar (subMatchingString current mapping)
subMonoTypeS (TApp n ts) mapping = TApp n (fmap (`subMonoTypeS` mapping) ts)
subMonoTypeS t mapping = t

findFree :: Context -> MonoType -> Set String
findFree c (TConst s) = Data.Set.empty
findFree c (TVar s) = case Map.lookup s c of
    Just _ -> Data.Set.empty
    Nothing -> Data.Set.singleton s
findFree c (TApp "->" [e1, e2]) = findFree c e1 `Data.Set.union` findFree c e2
findFree c (TApp _ es) = foldl (\prev e -> prev `Data.Set.union` findFree c e) Data.Set.empty es
findFree _ Unknown = Data.Set.empty

-- This is gamma bar in the let rule (Γ)
generalize :: State -> Context -> MonoType -> (PolyType, State)
generalize state c mt =
    let freeVariables = Data.Set.toList $ findFree c mt in
    let (state', substitutions) = Data.List.foldl (\(state, subs) free -> let (nmt, state') = newVar state in (state', subs ++ [(TVar free, nmt)])) (state, []) freeVariables in
    let newNames = fmap (\(_, TVar name) -> name) substitutions in
    (TPoly newNames (subMultipleMonoTypeT mt substitutions), state')

contains :: MonoType -> MonoType -> Bool
contains (TApp _ ts) b = any (contains b) ts
contains a b = a == b

-- mgu
mostGeneralUnifier :: MonoType -> MonoType -> Expr -> Either TypeSubstitution TypeErrorMessage
mostGeneralUnifier a@(TConst _) b@(TConst _) e = if a == b then Left [] else Right $ MguMismatch a b e
mostGeneralUnifier a@(TVar _) b@(TConst _) e = Left [(a, b)]
mostGeneralUnifier a@(TConst _) b@(TVar _) e = Left [(b, a)]
mostGeneralUnifier a@(TConst _) b@(TApp _ _) e = Right $ MguMismatch a b e
mostGeneralUnifier a@(TApp _ _) b@(TConst _) e = Right $ MguMismatch b a e
mostGeneralUnifier a@(TVar _) b e
    | a == b = Left []
    | b `contains` a = Right (MguInfiniteType a b e)
    | otherwise = Left [(a, b)]
mostGeneralUnifier a b@(TVar _) e = mostGeneralUnifier b a e
mostGeneralUnifier a@(TApp n1 a1) b@(TApp n2 a2) e =
    if n1 /= n2 || length a1 /= length a2 then Right (MguDifferentTypeFunctions a b e) else
    Data.List.foldl zipSub (Left []) (zip a1 a2)
        where
            zipSub :: Either TypeSubstitution TypeErrorMessage -> (MonoType, MonoType) -> Either TypeSubstitution TypeErrorMessage
            zipSub (Right e) p = Right e
            zipSub (Left s) (a, b) = case mostGeneralUnifier (subMultipleMonoTypeT a s) (subMultipleMonoTypeT b s) e of
                Left newSubs -> Left $ s ++ newSubs
                e -> e
mostGeneralUnifier a b e = error ("Tried to unify " ++ showTMonoE a ++ " and " ++ showTMonoE b)

-- This is a hack we should not deal with polymorphic types outside let bindings and variable instantiation
toMono (TPoly [] m) = m
toPoly m = TPoly [] m

-- https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
-- TODO algorithmW should only produce mono types and only the let rule should produce one polytype internally
-- TODO Change Context to String -> Either PolyType MonoType and abolish toPoly and toMono
algorithmW :: State -> Context -> Expr -> (TypedExpr, PolyType, TypeSubstitution, State)
algorithmW state c (Var term) =
    let (state', maybeType) = typeVar state c term in
    case maybeType of
        Just t -> (TypedVar term t, t, [], state')
        Nothing -> (TypeError (UnboundVar term), polyUnknown, [], state')
algorithmW state c pe@(App [e1, e2]) =
    let (typedE1, t1, s1, state') = algorithmW state c e1 in case typedE1 of
        TypeError _ -> (typedE1, t1, s1, state')
        _ ->
            let (t', state'') = newVar state' in
            let sContext = substituteInContextT c s1 in
            let (typedE2, t2, s2, state''') = algorithmW state'' sContext e2 in case typedE2 of
                TypeError _ -> (typedE2, t2, s1 ++ s2, state''')
                _ ->
                    let mguS3 = (mostGeneralUnifier (toMono t1) ((TApp "->" [toMono t2, t'])) (pe)) in -- TODO should we sub on t1 here?
                    case mguS3 of
                        Right error -> (TypeError error, polyUnknown, [], state''')
                        Left s3 -> let subbedT' = subMultipleMonoTypeT t' s3 in (TypedApp [typedE1, typedE2] (toPoly subbedT'), toPoly subbedT', s1 ++ s2 ++ s3, state''')
algorithmW state c (App exprs) = algorithmW state c (unfold exprs)
    where
        unfold [] = error "Application with no expression is malformed"
        unfold [e1] = e1
        unfold [e1, e2] = App [e1, e2]
        unfold exprs = App [unfold (init exprs), last exprs]
algorithmW state c (Abs name e) =
    let (t, state') = newVar state in
    let newContext = insert name (toPoly t) c in
    let (typedE, t', s, state'') = algorithmW state' newContext e in
    let fnType = toPoly (TApp "->" [t, toMono t']) in
    (TypedAbs name t typedE fnType, fnType, s, state'')
algorithmW state c (Let name e1 e2) =
    let (typedE1, t1, s1, state') = algorithmW state c e1 in
    let sContext = substituteInContextT c s1 in
    let (gt, state'') = generalize state' sContext (subMultipleMonoTypeT (toMono t1) s1) in
    let newSContext = insert name gt sContext in
    let (typedE2, t2, s2, state''') = algorithmW state'' newSContext e2 in
    (TypedLet name gt typedE1 typedE2 t2, t2, s1 ++ s2, state''')
algorithmW state c (LetTop term e) =
    -- TODO treat this as a normal let, i.e. rewrite program as a series of lets instead
    let (typedE, t, s, state') = algorithmW state c e in
    (TypedLetTop term typedE t, t, s, state')
algorithmW state c (Enclosed e) = algorithmW state c e
-- TODO remove these
algorithmW state c NewLine = (NoExpr, polyUnknown, [], state)
algorithmW state c (ParseError m) = (TypeError UnexpectedParseError, polyUnknown, [], state)

-- This needs to pass along substitutions as well
typeAndAddToContext :: ([TypedExpr], State, TypeSubstitution, Context) -> Expr -> ([TypedExpr], State, TypeSubstitution, Context)
typeAndAddToContext (texprs, state, s1, c) e =
    let sContext = substituteInContextT c s1 in
    let (typedE, t, s2, state') = algorithmW state sContext e in
    case typedE of
        (TypedLetTop name _ t) ->
            let sContext' = substituteInContextT sContext s2 in
            let (gt, state'') = generalize state' sContext' (subMultipleMonoTypeT (toMono t) s2) in
            let newContext = insert name gt sContext' in
            (texprs ++ [typedE], state'', s1 ++ s2, newContext)
        NoExpr -> (texprs ++ [NoExpr], state', s1 ++ s2, c)
        (TypeError UnexpectedParseError) -> (texprs ++ [NoExpr], state', s1, c)
        _ -> (texprs ++ [TypeError NoTopLevelBinding], state', s1, c)

builtinFunctions =
    [ ("print",       TPoly [] (TApp "->" [strType, strType]))
    , ("numToString", TPoly [] (TApp "->" [numType, strType]))
    , ("toString",    TPoly ["a"] (TApp "->" [TVar "a", strType]))
    , ("add",         TPoly [] (TApp "->" [numType, TApp "->" [numType, numType]]))
    ]

typed :: [Expr] -> ([TypedExpr], TypeSubstitution, Context)
typed exprs =
    let initialState = (Data.Char.ord 'a', 0) in
    let context = Map.fromList builtinFunctions in
    let (typedExprs, _, s, c) = Data.List.foldl typeAndAddToContext ([], initialState, [], context) exprs in
    (typedExprs, s, c)

applySub :: [TypedExpr] -> TypeSubstitution -> [TypedExpr]
applySub exprs s = fmap (`applySubstitutionToTypedExpr` s) exprs

applySubstitutionToTypedExpr :: TypedExpr -> TypeSubstitution -> TypedExpr
applySubstitutionToTypedExpr (TypedVar v t) s = TypedVar v (subMultiplePolyTypeT t s)
applySubstitutionToTypedExpr (TypedApp exprs t) s = TypedApp (fmap (`applySubstitutionToTypedExpr` s) exprs) (subMultiplePolyTypeT t s)
applySubstitutionToTypedExpr (TypedAbs arg argType e t) s = TypedAbs arg (subMultipleMonoTypeT argType s) (applySubstitutionToTypedExpr e s) (subMultiplePolyTypeT t s)
applySubstitutionToTypedExpr (TypedLet name nameType e1 e2 t) s = TypedLet name (subMultiplePolyTypeT nameType s) (applySubstitutionToTypedExpr e1 s) (applySubstitutionToTypedExpr e2 s) (subMultiplePolyTypeT t s)
applySubstitutionToTypedExpr (TypedLetTop name e t) s = TypedLetTop name (applySubstitutionToTypedExpr e s) (subMultiplePolyTypeT t s)
applySubstitutionToTypedExpr a _ = a
-- applySubstitutionToTypedExpr (TypeError TypeErrorMessage)
-- applySubstitutionToTypedExpr NoExpr = NoExpr

collectTypeError :: [TypeErrorMessage] -> TypedExpr -> [TypeErrorMessage]
collectTypeError prev n@(TypedApp exprs _) = Data.List.foldl collectTypeError prev exprs
collectTypeError prev n@(TypedAbs _ _ e _) = collectTypeError prev e
collectTypeError prev n@(TypedLet _ _ e e2 t) = collectTypeError (collectTypeError prev e) e2
collectTypeError prev n@(TypedLetTop _ e _) = collectTypeError prev e
collectTypeError prev (TypeError m) = prev ++ [m]
collectTypeError prev e = prev

collectTypeErrors :: [TypedExpr] -> [String]
collectTypeErrors exprs = fmap showTypeError $ Data.List.foldl collectTypeError ([] :: [TypeErrorMessage]) exprs

