module Context where

import Data.Fin
import Data.Nat
import Data.Vect
import Values
import Weaken
import Terms


data Context (vars :: Nat) (frees :: Nat) = Context {
    types  :: Env vars frees,
    stages :: Vect vars Stage,
    values :: Env vars frees,
    frees  :: SNat frees
}


initial :: Context Z Z
initial = Context Nil Nil Nil SZ


extendFree :: Value frees -> Stage -> Context vars frees -> Context (S vars) (S frees)
extendFree ty n (Context types stages values frees) = Context
    (weakenEnv (Cons ty types))
    (Cons n stages)
    (Cons (VVar (limit frees)) (weakenEnv values))
    (SS frees)


extendBound :: Value frees -> Value frees -> Stage -> Context vars frees -> Context (S vars) frees
extendBound ty val n (Context types stages values frees) = Context
    (Cons ty types)
    (Cons n stages)
    (Cons val values)
    frees