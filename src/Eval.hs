module Eval where

import Data.Fin
import Data.Nat
import Data.Vect
import Terms
import Values
import Weaken
import Control.Monad.Fix (fix)


-- Expressions are evaluated in an environment to their values.

eval :: Env from to -> Expr from -> Value to
eval env (Var v)      = level v env
eval env (Lam x)      = VLam $ Lazily env x
eval env (App x y)    = apply (eval env x) (eval env y)
eval env (Let _ x y)  = eval env (App (Lam y) x)
eval env (Pi x n y m) = VPi (eval env x) n (Lazily env y) m
eval env (Sigma x y)  = VSigma (eval env x) (Lazily env y)
eval env (Pair x y)   = VPair (eval env x) (eval env y)
eval env (Fst x)      = projectFst (eval env x)
eval env (Snd x)      = projectSnd (eval env x)
eval env (Ano x _)    = eval env x
eval env Type         = VType
eval env (Const c)    = VConst c
eval env (Run n x)    = VRun n (eval env x)
eval env (If b t f)   = doIf (eval env b) (eval env t) (eval env f)
eval env (Add x y)    = doAdd (eval env x) (eval env y)
eval env (Sub x y)    = doSub (eval env x) (eval env y)
eval env (Fix x)      = fix $ \res -> eval (Cons res env) x


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


doIf :: Value vars -> Value vars -> Value vars -> Value vars
doIf (VConst (IntLit n)) t f = if n > 0 then t else f
doIf (VRun _ (VConst (IntLit n))) t f = if n > 0 then t else f
doIf b t f = VIf b t f


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
force (Lazily env x) arg = eval (Cons arg env) x

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


-- Normalisation is just the composition of evaluation and reification.
-- Most of the time when calling this function, 'from' and 'to' will be
-- the same.

norm :: SNat to -> Env from to -> Expr from -> Expr to
norm to env x = reify to $ eval env x
