module Eval where

import Data.Fin
import Data.Nat
import Data.Vect
import Terms
import Values
import Weaken
import Control.Monad.Fix (fix)


-- Expressions are eval' stuckuated in an environment to their values.

eval :: Env from to -> Expr from -> Value to
eval = eval' False

eval' :: Bool -> Env from to -> Expr from -> Value to
eval' stuck env (Var v)      = level v env
eval' stuck env (Lam x)      = VLam $ Lazily stuck env x
eval' stuck env (App x y)    = apply (eval' stuck env x) (eval' stuck env y)
eval' stuck env (Let _ x y)  = eval' stuck env (App (Lam y) x)
eval' stuck env (Pi x n y m) = VPi (eval' stuck env x) n (Lazily stuck env y) m
eval' stuck env (Sigma x y)  = VSigma (eval' stuck env x) (Lazily stuck env y)
eval' stuck env (Pair x y)   = VPair (eval' stuck env x) (eval' stuck env y)
eval' stuck env (Fst x)      = projectFst (eval' stuck env x)
eval' stuck env (Snd x)      = projectSnd (eval' stuck env x)
eval' stuck env (Ano x _)    = eval' stuck env x
eval' stuck env Type         = VType
eval' stuck env (Const c)    = VConst c
eval' stuck env (Run n x)    = VRun n (eval' stuck env x)
eval' stuck env (If b t f)   = doIf stuck env (eval' stuck env b) t f
eval' stuck env (Add x y)    = doAdd (eval' stuck env x) (eval' stuck env y)
eval' stuck env (Sub x y)    = doSub (eval' stuck env x) (eval' stuck env y)
eval' stuck env (Fix x)      = fix $ \res -> eval' stuck (Cons res env) x


-- Try to apply closures if possible..

apply :: Value vars -> Value vars -> Value vars
apply (VLam clos) arg = force clos arg
apply x           y   = VApp x y


projectFst :: Value vars -> Value vars
projectFst (VPair x y) = x
projectFst e           = VFst e


projectSnd :: Value vars -> Value vars
projectSnd (VPair x y) = y
projectSnd e           = VSnd e


doIf :: Bool -> Env from to -> Value to -> Expr from -> Expr from -> Value to
doIf stuck env (VConst (IntLit n)) t f = if n > 0 then eval' stuck env t else eval' stuck env f
doIf stuck env (VRun _ (VConst (IntLit n))) t f = if n > 0 then eval' stuck env t else eval' stuck env f
doIf stuck env b t f = VIf b (eval' True env t) (eval' True env f)


doAdd :: Value vars -> Value vars -> Value vars
doAdd (VConst (IntLit n)) (VConst (IntLit m)) = VConst (IntLit (n + m))
doAdd (VRun i (VConst (IntLit n))) (VRun _ (VConst (IntLit m))) = VRun i $ VConst (IntLit (n + m))
doAdd n m = VAdd n m


doSub :: Value vars -> Value vars -> Value vars
doSub (VConst (IntLit n)) (VConst (IntLit m)) = VConst (IntLit (n - m))
doSub (VRun i (VConst (IntLit n))) (VRun _ (VConst (IntLit m))) = VRun i $ VConst (IntLit (n - m))
doSub n m = VSub n m


-- When the value of a closure is required, we can force it by giving
-- the required argument.

force :: Closure vars -> Value vars -> Value vars
force (Lazily stuck env x) arg = eval' stuck (Cons arg env) x

forceFresh :: SNat vars -> Closure vars -> Value (S vars)
forceFresh vars clos = force (weakenClosure clos) (VVar (limit vars))


-- To complete normalisation, we must now convert the values back into
-- normal expressions.

reify :: SNat vars -> Value vars -> Expr vars
reify vars (VVar v)      = Var v
reify vars (VApp x y)    = App (reify vars x) (reify vars y)
reify vars (VLam clos)   = Lam (reifyClosure vars clos)
reify vars (VPi x n y m) = Pi (reify vars x) n (reifyClosure vars y) m
reify vars (VSigma x y)  = Sigma (reify vars x) (reifyClosure vars y)
reify vars (VPair x y)   = Pair (reify vars x) (reify vars y)
reify vars (VFst x)      = Fst (reify vars x)
reify vars (VSnd x)      = Snd (reify vars x)
reify vars VType         = Type
reify vars (VConst c)    = Const c
reify vars (VRun n x)    = Run n (reify vars x)
reify vars (VIf b t f)   = If (reify vars b) (reify vars t) (reify vars f)
reify vars (VAdd x y)    = Add (reify vars x) (reify vars y)
reify vars (VSub x y)    = Sub (reify vars x) (reify vars y)
reify vars (VFix x)      = Fix (reifyClosure vars x)


reifyClosure :: SNat vars -> Closure vars -> Expr (S vars)
reifyClosure vars clos = reify (SS vars) (forceFresh vars clos)


-- Normalisation is just the composition of eval' stuckuation and reification.
-- Most of the time when calling this function, 'from' and 'to' will be
-- the same.

norm :: SNat to -> Env from to -> Expr from -> Expr to
norm to env x = reify to $ eval env x
