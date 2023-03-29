module Check where

import Context 
import Data.Nat
import Data.Vect
import Terms
import Values
import Eval
import Control.Monad (unless)


infer :: Context vars frees -> Expr vars -> Maybe (Value frees, Stage)
infer ctx (Var lvl)    = Just (level lvl (types ctx), level lvl (stages ctx))
infer ctx (App x y)    = inferApp ctx x y
infer ctx (Lam x)      = Nothing
infer ctx (Let x y)    = inferLet ctx x y
infer ctx (Pi x n y m) = inferPi ctx x y
infer ctx (Sigma x y)  = inferPi ctx x y
infer ctx (Pair x y)   = Nothing
infer ctx (Fst x)      = inferFst ctx x
infer ctx (Snd x)      = inferSnd ctx x
infer ctx (Ano x t)    = inferAno ctx x t
infer ctx Type         = Just (VType, Constant)
infer ctx (Const c)    = Just (inferConst c, Constant)


check :: Context vars frees -> Expr vars -> Value frees -> Maybe Stage -> Maybe Stage
check ctx (Lam x)    exp n = checkLam ctx x exp n
check ctx (Pair x y) exp n = checkPair ctx x y exp n
check ctx expr    expected  n = do
    (actual, m) <- infer ctx expr
    let expected' = reify (frees ctx) expected
    let actual'   = reify (frees ctx) actual
    unless (expected' == actual') Nothing

    case n of
        Just n | n /= m -> Nothing
        _ -> pure m



checkLam :: Context vars frees -> Expr (S vars) -> Value frees -> Maybe Stage -> Maybe Stage
checkLam ctx x (VPi t n u m) l = do
    check (extendFree t n ctx) x (forceFresh (frees ctx) u) (Just m)
    pure Constant
checkLam ctx x _ l = Nothing


checkPair :: Context vars frees -> Expr vars -> Expr vars -> Value frees -> Maybe Stage -> Maybe Stage
checkPair ctx x y (VSigma t u) l = do
    n <- check ctx x t l
    m <- check ctx y (force u (eval (values ctx) x)) l
    pure (n <> m)
checkPair ctx x y _ l = Nothing


inferApp :: Context vars frees -> Expr vars -> Expr vars -> Maybe (Value frees, Stage)
inferApp ctx lhs rhs = infer ctx lhs >>= \case
    (VPi from n to m, l) -> do
        check ctx rhs from (Just n)
        let rhs' = eval (values ctx) rhs
        Just (force to rhs', l <> m)
    _ -> Nothing


inferLet :: Context vars frees -> Expr vars -> Expr (S vars) -> Maybe (Value frees, Stage)
inferLet ctx arg body = do
    (argTy, argStage) <- infer ctx arg
    let arg' = eval (values ctx) arg
    infer (extendBound argTy arg' argStage ctx) body


inferPi :: Context vars frees -> Expr vars -> Expr (S vars) -> Maybe (Value frees, Stage)
inferPi ctx from to = do
    n <- check ctx from VType Nothing
    let from' = eval (values ctx) from
    m <- check (extendFree from' n ctx) to VType Nothing
    Just (VType, n <> m)


inferAno :: Context vars frees -> Expr vars -> Expr vars -> Maybe (Value frees, Stage)
inferAno ctx x t = do
    check ctx t VType Nothing
    let t' = eval (values ctx) t
    n <- check ctx x t' Nothing
    Just (t', n)


inferFst :: Context vars frees -> Expr vars -> Maybe (Value frees, Stage)
inferFst ctx x = infer ctx x >>= \case
    (VSigma t u, n) -> pure (t, n)
    _ -> Nothing
    

inferSnd :: Context vars frees -> Expr vars -> Maybe (Value frees, Stage)
inferSnd ctx x = infer ctx x >>= \case
    (VSigma t u, n) -> pure (force u $ eval (values ctx) (Fst x), n)
    _ -> Nothing


inferConst :: Const -> Value vars
inferConst Int        = VType
inferConst (IntLit _) = VConst Int