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

data Literal = IntLit Int | UnitLit

data QualifiedType = QualifiedType {t :: Type, reachable :: Maybe (Set Name)}

data Type
    = Unit
    | Int
    | Ref Type
    | Fun
        { funName :: Name
        , param :: Name
        , paramType :: QualifiedType
        , returnType :: QualifiedType
        }
