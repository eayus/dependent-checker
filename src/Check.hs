module Check where

import Context 
import Data.Nat
import Data.Vect
import Terms
import Values
import Eval
import Control.Monad (unless)


infer :: Context vars frees -> Expr vars -> Maybe (Value frees)
infer ctx (Var lvl)   = Just (level lvl (types ctx))
infer ctx (App x y)   = inferApp ctx x y
infer ctx (Lam x)     = Nothing
infer ctx (Let x y)   = inferLet ctx x y
infer ctx (Pi x y)    = inferPi ctx x y
infer ctx (Sigma x y) = inferPi ctx x y
infer ctx (Pair x y)  = Nothing
infer ctx (Ano x t)   = inferAno ctx x t
infer ctx Type        = Just VType


check :: Context vars frees -> Expr vars -> Value frees -> Maybe ()
check ctx (Lam x)    exp = checkLam ctx x exp
check ctx (Pair x y) exp = checkPair ctx x y exp
check ctx expr    expected  = do
    actual <- infer ctx expr
    let expected' = reify (frees ctx) expected
    let actual'   = reify (frees ctx) actual
    unless (expected' == actual') Nothing


checkLam :: Context vars frees -> Expr (S vars) -> Value frees -> Maybe ()
checkLam ctx x (VPi t u) = check (extendFree t ctx) x (forceFresh (frees ctx) u)
checkLam ctx x _ = Nothing


checkPair :: Context vars frees -> Expr vars -> Expr vars -> Value frees -> Maybe ()
checkPair ctx x y (VSigma t u) = do
    check ctx x t
    check ctx y (force u (eval (values ctx) x))
checkPair ctx x y _ = Nothing


inferApp :: Context vars frees -> Expr vars -> Expr vars -> Maybe (Value frees)
inferApp ctx lhs rhs = infer ctx lhs >>= \case
    VPi from to -> do
        check ctx rhs from
        let rhs' = eval (values ctx) rhs
        Just (force to rhs')
    _ -> Nothing


inferLet :: Context vars frees -> Expr vars -> Expr (S vars) -> Maybe (Value frees)
inferLet ctx arg body = do
    argTy <- infer ctx arg
    let arg' = eval (values ctx) arg
    infer (extendBound argTy arg' ctx) body


inferPi :: Context vars frees -> Expr vars -> Expr (S vars) -> Maybe (Value frees)
inferPi ctx from to = do
    check ctx from VType
    let from' = eval (values ctx) from
    check (extendFree from' ctx) to VType
    Just VType


inferAno :: Context vars frees -> Expr vars -> Expr vars -> Maybe (Value frees)
inferAno ctx x t = do
    check ctx t VType
    let t' = eval (values ctx) t
    check ctx x t'
    Just t'