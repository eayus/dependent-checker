module Weaken where

import Data.Fin
import Data.Nat
import Data.Vect
import Values


-- Often we need to use a value in a larger context than under which it
-- was originally created. To do this, we must 'weaken' the index. 
-- 
-- Since we're using de Bruijn levels, all of the functions below are
-- actually not changing the data at all, and are just there to convince
-- Haskell that the index can be changed. In a production implementation,
-- these functions should not be called, and an unsafe type coercion should
-- be preferred for efficiency reasons.


weakenValue :: Value vars -> Value (S vars)
weakenValue (VVar v)      = VVar $ relax v
weakenValue (VApp x y)    = VApp (weakenValue x) (weakenValue y)
weakenValue (VLam clos)   = VLam $ weakenClosure clos
weakenValue (VPi x n y m) = VPi (weakenValue x) n (weakenClosure y) m
weakenValue (VSigma x y)  = VSigma (weakenValue x) (weakenClosure y)
weakenValue (VPair x y)   = VPair (weakenValue x) (weakenValue y)
weakenValue (VFst x)      = VFst (weakenValue x)
weakenValue (VSnd x)      = VSnd (weakenValue x)
weakenValue VType         = VType
weakenValue (VConst c)    = VConst c
weakenValue (VRun n x)    = VRun n (weakenValue x)
weakenValue (VIf b t f)   = VIf (weakenValue b) (weakenValue t) (weakenValue f) 
weakenValue (VAdd x y)    = VAdd (weakenValue x) (weakenValue y)
weakenValue (VSub x y)    = VSub (weakenValue x) (weakenValue y)


weakenEnv :: Env from to -> Env from (S to)
weakenEnv = fmap weakenValue


weakenClosure :: Closure vars -> Closure (S vars)
weakenClosure (Lazily env x) = Lazily (weakenEnv env) x
