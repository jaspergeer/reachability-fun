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

(<:) :: L.QualifiedType -> L.QualifiedType -> Bool
t₁ <: t₂ = undefined

filterEnv :: Maybe (Set Name) -> Map Name L.QualifiedType -> Map Name L.QualifiedType
filterEnv q = Map.filter (\(L.QualifiedType _ q') -> q' ⊑ q)

untracked :: L.Type -> L.QualifiedType
untracked t = L.QualifiedType t Nothing

tracked :: L.Type -> Set Name -> L.QualifiedType
tracked t = L.QualifiedType t . Just

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
                        L.QualifiedType _ reachable <- Map.lookup n gamma
                        return reachable
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
        if inferredReturnType <: returnType then return f' else Nothing

typeLiteral :: L.Literal -> L.QualifiedType
typeLiteral lit = case lit of
    L.IntLit _ -> untracked L.Int
    L.UnitLit -> untracked L.Unit

fv :: L.Term -> Set Name
fv term = case term of
    L.Constant _ -> Set.empty
    L.Var n -> Set.singleton n
    L.Abs selfRef param _ body _ -> Set.difference (fv body) (Set.fromList [selfRef, param])
    L.Apply t₁ t₂ -> Set.union (fv t₁) (fv t₂)
    L.Cell t -> fv t
    L.Deref t -> fv t
    L.Assign t₁ t₂ -> Set.union (fv t₁) (fv t₂)
