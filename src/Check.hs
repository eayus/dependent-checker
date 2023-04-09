module Check where

import Context 
import Data.Nat
import Data.Vect hiding ((++))
import Terms
import Values
import Eval
import Control.Monad (unless)
import Weaken (weakenValue)


infer :: Context vars frees -> Expr vars -> Either String (Value frees, Stage)
infer ctx (Var lvl)    = pure (level lvl (types ctx), level lvl (stages ctx))
infer ctx (App x y)    = inferApp ctx x y
infer ctx (Lam x)      = Left "Can't infer lam"
infer ctx (Let t x y)  = inferLet ctx t x y
infer ctx (Pi x n y m) = inferPi ctx x n y m
infer ctx (Sigma x y)  = inferPi ctx x Runtime y Runtime
infer ctx (Pair x y)   = Left "Can't infer pair"
infer ctx (Fst x)      = inferFst ctx x
infer ctx (Snd x)      = inferSnd ctx x
infer ctx (Ano x t)    = inferAno ctx x t
infer ctx Type         = pure (VType, Constant)
infer ctx (Const c)    = pure (inferConst c, Constant)
infer ctx (Run n x)    = inferRun ctx n x
infer ctx (If b t f)   = inferIf ctx b t f
infer ctx (Add x y)    = inferAddSub ctx x y
infer ctx (Sub x y)    = inferAddSub ctx x y
infer ctx (Fix x)      = Left "Cannot infer fix"


check :: Context vars frees -> Expr vars -> Value frees -> Maybe Stage -> Either String Stage
check ctx (Lam x)    exp n = checkLam ctx x exp n
check ctx (Pair x y) exp n = checkPair ctx x y exp n
check ctx (Fix x)    exp n = checkFix ctx x exp n
check ctx expr    expected  n = do
    (actual, m) <- infer ctx expr
    let expected' = reify (frees ctx) expected
    let actual'   = reify (frees ctx) actual
    unless (expected' == actual') (Left $ "Type mismatch: " ++ show expected' ++ "\n" ++ show actual')

    case n of
        Just n | n /= m -> Left $ "Stage mismatch for:\n" ++ show expr ++ "\nExpected " ++ show n ++ " but got " ++ show m
        _ -> pure m



checkLam :: Context vars frees -> Expr (S vars) -> Value frees -> Maybe Stage -> Either String Stage
checkLam ctx x (VPi t n u m) l = do
    check (extendFree t n ctx) x (forceFresh (frees ctx) u) (pure m)
    pure Constant
checkLam ctx x _ l = Left "Lam must have pi type"


checkPair :: Context vars frees -> Expr vars -> Expr vars -> Value frees -> Maybe Stage -> Either String Stage
checkPair ctx x y (VSigma t u) l = do
    n <- check ctx x t l
    m <- check ctx y (force u (eval (values ctx) x)) l
    pure (n <> m)
checkPair ctx x y _ l = Left "Pair must have sigma type"


checkFix :: Context vars frees -> Expr (S vars) -> Value frees -> Maybe Stage -> Either String Stage
checkFix ctx body ty Nothing = Left "Fix must know stage"
checkFix ctx body ty (Just n) = do
    let ctx' = extendFree ty n ctx
    check ctx' body (weakenValue ty) (Just n)
    pure n


inferApp :: Context vars frees -> Expr vars -> Expr vars -> Either String (Value frees, Stage)
inferApp ctx lhs rhs = infer ctx lhs >>= \case
    (VPi from n to m, l) -> do
        check ctx rhs from (pure n)
        let rhs' = eval (values ctx) rhs
        pure (force to rhs', l <> m)
    _ -> Left "Cannot apply term which does not have pi type"


inferLet :: Context vars frees -> Maybe (Expr vars, Stage) -> Expr vars -> Expr (S vars) -> Either String (Value frees, Stage)
inferLet ctx Nothing arg body = do
    (argTy, argStage) <- infer ctx arg
    let arg' = eval (values ctx) arg
    infer (extendBound argTy arg' argStage ctx) body
inferLet ctx (Just (t, n)) arg body = do
    check ctx t VType Nothing
    let t' = eval (values ctx) t
    check ctx arg t' (Just n)
    let arg' = eval (values ctx) arg
    infer (extendBound t' arg' n ctx) body


inferPi :: Context vars frees -> Expr vars -> Stage -> Expr (S vars) -> Stage -> Either String (Value frees, Stage)
inferPi ctx from fromS to toS = do
    n <- check ctx from VType Nothing
    let from' = eval (values ctx) from
    m <- check (extendFree from' fromS ctx) to VType Nothing
    pure (VType, n <> m)


inferAno :: Context vars frees -> Expr vars -> Expr vars -> Either String (Value frees, Stage)
inferAno ctx x t = do
    check ctx t VType Nothing
    let t' = eval (values ctx) t
    n <- check ctx x t' Nothing
    pure (t', n)


inferFst :: Context vars frees -> Expr vars -> Either String (Value frees, Stage)
inferFst ctx x = infer ctx x >>= \case
    (VSigma t u, n) -> pure (t, n)
    _ -> Left "Cannot project term not of sigma type"
    

inferSnd :: Context vars frees -> Expr vars -> Either String (Value frees, Stage)
inferSnd ctx x = infer ctx x >>= \case
    (VSigma t u, n) -> pure (force u $ eval (values ctx) (Fst x), n)
    _ -> Left "Cannot project term not of sigma type"


inferRun :: Context vars frees -> Int -> Expr vars -> Either String (Value frees, Stage)
inferRun ctx i x = do
    (t, n) <- infer ctx x
    pure $ case i of
        0 -> (t, Runtime)
        _ -> (VRun (i - 1) t, Runtime)


inferIf :: Context vars frees -> Expr vars -> Expr vars -> Expr vars -> Either String (Value frees, Stage)
inferIf ctx b t f = do
    (t, n) <- infer ctx t
    check ctx f t (Just n)
    check ctx b (VConst Int) (Just n)
    pure (t, n)


inferAddSub :: Context vars frees -> Expr vars -> Expr vars -> Either String (Value frees, Stage)
inferAddSub ctx x y = do
    n <- check ctx x (VConst Int) Nothing
    check ctx y (VConst Int) (Just n)
    pure (VConst Int, n)


inferConst :: Const -> Value vars
inferConst Int        = VType
inferConst (IntLit _) = VConst Int
inferConst Unit       = VType
inferConst It         = VConst Unit