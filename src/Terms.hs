module Terms where

import Util


-- Expressions are indexed by the number of variables in scope.
-- Variables are represented by 'de Bruijn levels'. (Not *indices*!)

data Expr (vars :: Nat)
    = Var (Fin vars) 
    | App (Expr vars) (Expr vars)
    | Lam (Expr (S vars))
    | Let { arg :: Expr vars, ty :: Expr vars, body :: Expr (S vars) }
    | Pi (Expr vars) (Expr (S vars))
    | Type
    deriving Eq


instance Show (Expr vars) where
    show (Var v)      = show $ finToInt v
    show (App x y)    = "(" ++ show x ++ " " ++ show y ++ ")"
    show (Lam x)      = "(Lam => " ++ show x ++ ")"
    show (Let x ty y) = "(Let " ++ show x ++ " : " ++ show ty ++ " => " ++ show y ++ ")"
    show (Pi x y)     = "(Π " ++ show x ++ " -> " ++ show y ++ ")"
    show Type         = "type"
