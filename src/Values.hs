module Values where

import Data.Fin
import Data.Nat
import Data.Vect
import Terms


-- Values are the semantic meaning for an expression.
-- We also use de Bruijn levels here.

data Value (vars :: Nat)
    = VVar (Fin vars)
    | VApp (Value vars) (Value vars)
    | VLam (Closure vars)
    | VPi (Value vars) (Closure vars)
    | VSigma (Value vars) (Closure vars)
    | VPair (Value vars) (Value vars)
    | VFst (Value vars)
    | VSnd (Value vars)
    | VType


-- Environments map one scope to another. They could be interepreted as a
-- form of explicit substitution. We use a left-extending environment,
-- but then index from the right to simulate de Bruijn levels.

type Env (from :: Nat) (to :: Nat) = Vect from (Value to)


-- A closure is a suspended computation. When evaluating lambda expressions,
-- we do not know whether the lambda will be applied yet. So we suspend the
-- evaluation in the closure.

data Closure :: Nat -> * where
    Lazily :: Env from vars -> Expr (S from) -> Closure vars
