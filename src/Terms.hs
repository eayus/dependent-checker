module Terms where

import Data.Fin
import Data.Nat

-- Expressions are indexed by the number of variables in scope.
-- Variables are represented by 'de Bruijn levels'. (Not *indices*!)

data Expr (vars :: Nat)
    = Var (Fin vars) 
    | App (Expr vars) (Expr vars)
    | Lam (Expr (S vars))
    | Let (Maybe (Expr vars, Stage)) (Expr vars) (Expr (S vars))
    | Pi (Expr vars) Stage (Expr (S vars)) Stage
    | Sigma (Expr vars) (Expr (S vars))
    | Pair (Expr vars) (Expr vars)
    | Fst (Expr vars)
    | Snd (Expr vars)
    | Ano (Expr vars) (Expr vars)
    | Type
    | Const Const
    | Run Int (Expr vars)
    | If (Expr vars) (Expr vars) (Expr vars)
    | Add (Expr vars) (Expr vars)
    | Sub (Expr vars) (Expr vars)
    | Fix (Expr (S vars))
    deriving (Eq, Show)


deriving instance Show (Fin vars)


data Const
    = Int
    | IntLit Integer
    deriving (Eq, Show)


data Stage
    = Constant
    | Runtime
    deriving (Eq, Show)


instance Semigroup Stage where
    Constant <> Constant = Constant
    _ <> _ = Runtime