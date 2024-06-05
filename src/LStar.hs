{-# LANGUAGE DuplicateRecordFields #-}

module LStar where

import Data.Set (Set)

type Name = String

data Term
    = Constant Literal
    | Var Name
    | Abs
        { funName :: Name
        , param :: Name
        , paramType :: QualifiedType
        , body :: Term
        , returnType :: QualifiedType
        }
    | Apply Term Term
    | Cell Term
    | Deref Term
    | Assign Term Term

data Literal = IntLit Int | UnitLit deriving (Eq)

data QualifiedType = QualifiedType {t :: Type, q :: Maybe (Set Name)} deriving (Eq)

data Type
    = Base Base
    | Ref Type
    | Fun
        { funName :: Name
        , param :: Name
        , paramType :: QualifiedType
        , returnType :: QualifiedType
        }
    deriving (Eq)

data Base = Unit | Int deriving (Eq)
