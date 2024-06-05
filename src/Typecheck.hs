{-# LANGUAGE OverloadedRecordDot #-}

module Typecheck where

import Data.Map qualified as Map
import Data.Set qualified as Set
import LStar qualified as L

type Name = String
type Map = Map.Map
type Set = Set.Set

infix 9 ⊕
infix 9 ⨆
infix 9 .+
infix 9 ⊑
infix 9 <:

(⊕) :: Maybe (Set Name) -> Maybe (Set Name) -> Maybe (Set Name)
Nothing ⊕ _ = Nothing
_ ⊕ Nothing = Nothing
(Just q₁) ⊕ (Just q₂) = Just $ Set.union q₁ q₂

(⨆) :: Maybe (Set Name) -> Maybe (Set Name) -> Maybe (Set Name)
Nothing ⨆ q₂ = q₂
q₁ ⨆ Nothing = q₁
(Just q₁) ⨆ (Just q₂) = Just $ Set.union q₁ q₂

(.+) :: Maybe (Set Name) -> Name -> Maybe (Set Name)
α .+ x = α ⊕ Just (Set.singleton x)

(⊑) :: Maybe (Set Name) -> Maybe (Set Name) -> Bool
Nothing ⊑ _ = True
_ ⊑ Nothing = False
(Just q₁) ⊑ (Just q₂) = Set.isSubsetOf q₁ q₂

-- TODO anytime we check if two types are equal really we should be doing this
(<:) :: L.QualifiedType -> L.QualifiedType -> (Map Name L.QualifiedType -> Bool)
τ₁ <: τ₂ = case (τ₁.t, τ₂.t) of
    (L.Ref t₁, L.Ref t₂) ->
        \gamma ->
            (untracked t₁ <: untracked t₂) gamma
                && (τ₁.q .<: τ₂.q) gamma
    (L.Base t₁, L.Base t₂) ->
        \gamma ->
            t₁ == t₂
                && (τ₁.q .<: τ₂.q) gamma
    (L.Fun f₁ x₁ qt₁ qt₂, L.Fun f₂ x₂ qt₃ qt₄) ->
        \gamma ->
            f₁ == f₂
                && x₁ == x₂
                && (τ₁.q .<: τ₂.q) gamma
                && (qt₃ <: qt₁) gamma
                && ( let gamma' =
                            Map.union gamma $
                                Map.fromList
                                    [ (f₁, (τ₁{L.q = τ₁.q .+ f₁}))
                                    , (x₁, (qt₃{L.q = qt₃.q .+ x₁}))
                                    ]
                      in (qt₂ <: qt₄) gamma'
                   )
  where
    normalize :: Map Name L.QualifiedType -> Set Name -> Set Name
    normalize gamma q =
        let expand n = case Map.lookup n gamma of
                Just (L.QualifiedType (L.Fun{}) (Just q₁)) -> if Set.isSubsetOf q₁ q then q else Set.union q q₁
                Just _ -> q
                Nothing -> q -- TODO maybe this should be an error
         in mconcat $ map expand (Set.toList q)

    (.<:) :: Maybe (Set Name) -> Maybe (Set Name) -> (Map Name L.QualifiedType -> Bool)
    q₁ .<: q₂ = \gamma -> (normalize gamma <$> q₁) ⊑ (normalize gamma <$> q₂)

filterEnv :: Maybe (Set Name) -> Map Name L.QualifiedType -> Map Name L.QualifiedType
filterEnv q = Map.filter (\(L.QualifiedType _ q') -> q' ⊑ q)

untracked :: L.Type -> L.QualifiedType
untracked t = L.QualifiedType t Nothing

tracked :: L.Type -> Set Name -> L.QualifiedType
tracked t = L.QualifiedType t . Just

-- TODO
initEnv :: Map Name L.QualifiedType
initEnv = Map.fromList []

typeOf :: Map Name L.QualifiedType -> L.Term -> Maybe L.QualifiedType
typeOf gamma term = case term of
    L.Constant lit -> pure $ typeLiteral lit
    L.Var n -> Map.lookup n gamma
    L.Cell term -> do
        L.QualifiedType t Nothing <- typeOf gamma term
        return $ tracked t Set.empty
    L.Deref term -> do
        L.QualifiedType (L.Ref t) _ <- typeOf gamma term
        return $ untracked t
    L.Abs funName param paramType body returnType -> do
        qf <-
            foldr (⨆) Nothing
                <$> traverse
                    ( \n -> do
                        L.QualifiedType _ q <- Map.lookup n gamma
                        return q
                    )
                    (Set.toList $ fv term)
        let L.QualifiedType t₁ q₁ = paramType
        let L.QualifiedType t₂ q₂ = returnType
        let f = L.Fun funName param paramType returnType
        let f' = L.QualifiedType f (qf .+ funName)

        let gamma' =
                filterEnv (Just $ Set.fromList [funName, param]) $
                    Map.union
                        gamma
                        ( Map.fromList
                            [ (funName, f')
                            , (param, L.QualifiedType t₁ (q₁ .+ param))
                            ]
                        )
        inferredReturnType <- typeOf gamma' body
        if (inferredReturnType <: returnType) gamma' then return f' else Nothing

typeLiteral :: L.Literal -> L.QualifiedType
typeLiteral lit = case lit of
    L.IntLit _ -> untracked $ L.Base L.Int
    L.UnitLit -> untracked $ L.Base L.Unit

fv :: L.Term -> Set Name
fv term = case term of
    L.Constant _ -> Set.empty
    L.Var n -> Set.singleton n
    L.Abs selfRef param _ body _ -> Set.difference (fv body) (Set.fromList [selfRef, param])
    L.Apply t₁ t₂ -> Set.union (fv t₁) (fv t₂)
    L.Cell t -> fv t
    L.Deref t -> fv t
    L.Assign t₁ t₂ -> Set.union (fv t₁) (fv t₂)
