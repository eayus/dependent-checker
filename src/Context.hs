module Context where

import Data.Fin
import Data.Nat
import Data.Vect
import Values
import Weaken


data Context (vars :: Nat) (frees :: Nat) = Context {
    types  :: Env vars frees,
    values :: Env vars frees,
    frees  :: SNat frees
}


initial :: Context Z Z
initial = Context Nil Nil SZ


extendFree :: Value frees -> Context vars frees -> Context (S vars) (S frees)
extendFree ty (Context types values frees) = Context
    (weakenEnv (Cons ty types))
    (Cons (VVar (limit frees)) (weakenEnv values))
    (SS frees)


extendBound :: Value frees -> Value frees -> Context vars frees -> Context (S vars) frees
extendBound ty val (Context types values frees) = Context
    (Cons ty types)
    (Cons val values)
    frees