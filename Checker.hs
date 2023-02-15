{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
module Checker (typed, showTypedExpr, collectTypeErrors) where

import Parser (Expr(..), Value(..))
import Data.Map as Map hiding (foldl)
import qualified Data.Maybe
import qualified Data.List
import qualified Data.Char
import Data.Bifunctor (first)
import Data.Set (Set, empty, singleton, union, toList)

data MonoType = TVar String | TApp String [MonoType] | Unknown
    deriving (Show, Eq)

boolType = TVar "Bool"
numType = TVar "Num"
strType = TVar "Str"

data PolyType =  TPoly [String] MonoType
    deriving (Show, Eq)

polyUnknown = TPoly [] Unknown

showTMono :: MonoType -> String
showTMono (TVar s) = s
showTMono (TApp "->" [t1@(TApp "->" _), t2]) = "(" ++ showTMono t1 ++ ") -> " ++ showTMono t2
showTMono (TApp "->" [t1, t2]) = showTMono t1 ++ " -> " ++ showTMono t2
showTMono (TApp "->" _) = error "Type application for function with more/less than two type arguments"
showTMono (TApp s ms) = s ++ " " ++ (unwords $ fmap showTMonoE ms)
showTMono Unknown = "_"

showTMonoE t@(TVar _) = showTMono t
showTMonoE t = "(" ++ showTMono t ++ ")"

showTPoly :: PolyType -> String
showTPoly (TPoly [] m) = showTMono m -- TODO dissallow polytypes without quantifiers. Non-empty list? -- type NonEmptyList a = (a, [a])
showTPoly (TPoly ss m) = unwords (fmap (\s -> "∀ " ++ s ++ " .") ss) ++ " " ++ showTMono m

showTPolyE t@(TPoly [] m) = showTMonoE m
showTPolyE t = "(" ++ showTPoly t ++ ")"

occursM :: String -> MonoType -> Bool
occursM s (TVar c) = s == c
occursM s (TApp "->" [e1, e2]) = occursM s e1 || occursM s e2
occursM s (TApp m ms) = any (occursM s) ms

-- occursP :: String -> PolyType -> Bool
-- occursP s (TPoly _ p) = _1 -- TODO occursP s p
--
-- normalizeQuantified :: PolyType -> PolyType
-- normalizeQuantified (TPoly s p) = if occursP s p then TPoly s (normalizeQuantified p) else normalizeQuantified p
-- normalizeQuantified e = e

data TypeErrorMessage = ApplicationMismatch PolyType PolyType | ArgumentMismatch PolyType PolyType | NoTopLevelBinding | UnexpectedParseError | UnboundVar Value | MguInfiniteType MonoType MonoType | MguDifferentTypeFunctions MonoType MonoType
    deriving Show

data TypedExpr = TypedVar Value PolyType | TypedApp [TypedExpr] PolyType | TypedAbs String TypedExpr PolyType | TypedLet String PolyType TypedExpr TypedExpr PolyType | TypedLetTop String TypedExpr PolyType | NoExpr | TypeError TypeErrorMessage
    deriving Show

showType = showTPoly
showTypeE = showTPolyE

showTypeError (ApplicationMismatch ft arg) = "Type error: tried to apply argument of type " ++ showTypeE arg ++ " to non function of type " ++ showTypeE ft ++ ""
showTypeError (ArgumentMismatch param arg) = "Type error: tried to apply argument of type " ++ showTypeE arg ++ " to function that takes a " ++ showTypeE param ++ ""
showTypeError NoTopLevelBinding = "Type error: Encountered something other than a binding at the top level"
showTypeError UnexpectedParseError = "Compiler error: Encountered parsing error in checker"
showTypeError (UnboundVar v) = "Type error: found unbound variable " ++ show v
showTypeError (MguInfiniteType a b) = "Type error: found infinite type in unification of " ++ showTMonoE a ++ " and " ++ showTMonoE b
showTypeError (MguDifferentTypeFunctions a b) = "Type error: can not unify differently structured type functions " ++ showTMonoE a ++ " and " ++ showTMonoE b

showTypedExpr :: TypedExpr -> String
showTypedExpr (TypedAbs s e t) = "(λ " ++ s ++ " . " ++ showTypedExpr e ++ "):" ++ showTypeE t
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
typeOf (TypedAbs _ _ t) = t
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
inst state (TPoly quantifiers m) = let (state', subs) = Data.List.foldl createSubs (state, []) quantifiers in (state', subMultipleMonoType m subs)
    where
        createSubs :: (State, [(String, String)]) -> String -> (State, [(String, String)])
        createSubs (state, prev) s = let (new, state') = newInstName state in (state', prev ++ [(s, new)])


type Substitution a = [(a, a)]
type StringSubstitution = Substitution String
type TypeSubstitution = Substitution MonoType

substituteInContextT :: TypeSubstitution -> Context -> Context
substituteInContextT subs c = Data.List.foldl sub1 c subs
    where
        sub1 :: Context -> (MonoType, MonoType) -> Context
        sub1 c mapping = Map.map (`subPolyTypeT` mapping) c

subPolyTypeT :: PolyType -> (MonoType, MonoType) -> PolyType
subPolyTypeT (TPoly quantifiers mt) mapping = TPoly quantifiers (subMonoTypeT mt mapping) -- Should this substitute in qualifiers? Probably but how? That is only possible if 'to' is a variable and not a compound type

subMonoTypeT :: MonoType -> (MonoType, MonoType) -> MonoType
subMonoTypeT (TApp n ts) mapping = TApp n (fmap (`subMonoTypeT` mapping) ts)
subMonoTypeT current (from, to) = if current == from then to else current

subMultipleMonoTypeT :: MonoType -> [(MonoType, MonoType)] -> MonoType
subMultipleMonoTypeT m mappings = Data.List.foldl subMonoTypeT m mappings



substituteInContext :: StringSubstitution -> Context -> Context
substituteInContext subs c = Data.List.foldl sub1 c subs
    where
        sub1 :: Context -> (String, String) -> Context
        sub1 c mapping = Map.map (`subPolyType` mapping) c

subMatchingString :: String -> (String, String) -> String
subMatchingString current (from, to) = if current == from then to else current

subPolyType :: PolyType -> (String, String) -> PolyType
subPolyType (TPoly quantifiers mt) mapping = TPoly (fmap (`subMatchingString` mapping) quantifiers) (subMonoType mt mapping)

subMultipleMonoType :: MonoType -> [(String, String)] -> MonoType
subMultipleMonoType m mappings = Data.List.foldl subMonoType m mappings

subMonoType :: MonoType -> (String, String) -> MonoType
subMonoType (TVar current) mapping = TVar (subMatchingString current mapping)
subMonoType (TApp "->" [m1, m2]) mapping = TApp "->" [subMonoType m1 mapping, subMonoType m2 mapping]
subMonoType t _ = t -- TODO sub in application as well

findFree :: Context -> MonoType -> Set String
findFree c (TVar s) = case Map.lookup s c of
    Just _ -> Data.Set.empty
    Nothing -> Data.Set.singleton s
findFree c (TApp "->" [e1, e2]) = findFree c e1 `Data.Set.union` findFree c e2
findFree c (TApp _ es) = foldl (\prev e -> prev `Data.Set.union` findFree c e) Data.Set.empty es
findFree _ Unknown = Data.Set.empty

-- This is gamma bar in the let rule (Γ)
generalize :: Context -> PolyType -> PolyType -- TODO ML should be Mono -> Poly probably?
generalize c (TPoly xs mt) = TPoly (Data.Set.toList $ findFree c mt) mt -- TODO perform substitution as well, or return it?

contains :: MonoType -> MonoType -> Bool
contains (TApp _ ts) b = any (contains b) ts
contains a b = a == b

-- mgu
mostGeneralUnifier :: MonoType -> MonoType -> Either TypeSubstitution TypeErrorMessage
mostGeneralUnifier a@(TVar _) b
    | a == b = Left []
    | b `contains` a = Right (MguInfiniteType a b)
    | otherwise = Left [(a, b)]
mostGeneralUnifier a b@(TVar _) = mostGeneralUnifier b a
mostGeneralUnifier a@(TApp n1 a1) b@(TApp n2 a2) =
    if n1 /= n2 || length a1 /= length a2 then Right (MguDifferentTypeFunctions a b) else
    Data.List.foldl zipSub (Left []) (zip a1 a2)
        where
            zipSub :: Either TypeSubstitution TypeErrorMessage -> (MonoType, MonoType) -> Either TypeSubstitution TypeErrorMessage
            zipSub (Right e) p = Right e
            zipSub (Left s) (a, b) = mostGeneralUnifier (subMultipleMonoTypeT a s) (subMultipleMonoTypeT b s)
mostGeneralUnifier a b = error ("Tried to unify " ++ showTMonoE a ++ " and " ++ showTMonoE b)

-- This is a hack we should not deal with polymorphic types outside let bindings and variable instantiation
toMono (TPoly [] m) = m
toPoly m = TPoly [] m

-- https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
-- TODO algorithmW should only produce mono types and only the let rule should produce one polytype internally
-- TODO Change Context to String -> Either PolyType MonoType and abolish toPoly and toMono
algorithmW :: State -> Context -> Expr -> (TypedExpr, PolyType, TypeSubstitution, State)
algorithmW state c (Var term _) =
    let (state', maybeType) = typeVar state c term in
    case maybeType of
        Just t -> (TypedVar term t, t, [], state')
        Nothing -> (TypeError (UnboundVar term), polyUnknown, [], state')
algorithmW state c (App [e1, e2] _) =
    let (typedE1, t1, s1, state') = algorithmW state c e1 in
    let (t', state'') = newVar state' in
    let sContext = substituteInContextT s1 c in
    let (typedE2, t2, s2, state''') = algorithmW state'' sContext e2 in
    let mguS3 = mostGeneralUnifier (toMono t1) (TApp "->" [toMono t2, t']) in
    case mguS3 of
        Right error -> (TypeError error, polyUnknown, [], state''')
        -- TODO perform s3 substituteInContext on t'
        Left s3 -> (TypedApp [typedE1, typedE2] (toPoly t'), toPoly t', s1 ++ s2 ++ s3, state''')
algorithmW state c (App _ _) = error "we only support single application right now"
algorithmW state c (Abs name _ e) =
    let (t, state') = newVar state in
    let newContext = insert name (toPoly t) c in
    let (typedE, t', s, state'') = algorithmW state' newContext e in
    let fnType = toPoly (TApp "->" [t, toMono t']) in
    (TypedAbs name typedE fnType, fnType, s, state'')
algorithmW state c (Let name _ e1 e2) =
    let (typedE1, t1, s1, state') = algorithmW state c e1 in
    let sContext = substituteInContextT s1 c in
    let gt = generalize sContext t1 in
    let newContext = insert name gt sContext in
    let (typedE2, t2, s2, state'') = algorithmW state' newContext e2 in
    (TypedLet name gt typedE1 typedE2 t2, t2, s1 ++ s2, state'')
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
    let context = insert "print" (TPoly [] (TApp "->" [strType, strType])) Map.empty in
    let (typedExprs, _, _) = Data.List.foldl typeAndAddToContext ([], initialState, context) exprs in typedExprs

collectTypeError :: [TypeErrorMessage] -> TypedExpr -> [TypeErrorMessage]
collectTypeError prev n@(TypedApp exprs _) = Data.List.foldl collectTypeError prev exprs
collectTypeError prev n@(TypedAbs _ e _) = collectTypeError prev e
collectTypeError prev n@(TypedLet _ _ e e2 t) = collectTypeError (collectTypeError prev e) e2
collectTypeError prev n@(TypedLetTop _ e _) = collectTypeError prev e
collectTypeError prev (TypeError m) = prev ++ [m]
collectTypeError prev e = prev

collectTypeErrors :: [TypedExpr] -> [String]
collectTypeErrors exprs = fmap showTypeError $ Data.List.foldl collectTypeError ([] :: [TypeErrorMessage]) exprs

