module Terms where

import Data.Fin
import Data.Nat

-- Expressions are indexed by the number of variables in scope.
-- Variables are represented by 'de Bruijn levels'. (Not *indices*!)

data Expr (vars :: Nat)
    = Var (Fin vars) 
    | App (Expr vars) (Expr vars)
    | Lam (Expr (S vars))
    | Let (Expr vars) (Expr (S vars))
    | Pi (Expr vars) (Expr (S vars))
    | Sigma (Expr vars) (Expr (S vars))
    | Pair (Expr vars) (Expr vars)
    | Fst (Expr vars)
    | Snd (Expr vars)
    | Ano (Expr vars) (Expr vars)
    | Type
    | Const Const
    deriving Eq


data Const
    = Int
    | IntLit Integer
    deriving Eq