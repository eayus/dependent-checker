module Weaken where

import Util
import Terms
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
weakenValue (VVar v)     = VVar $ weakenFin v
weakenValue (VApp x y)   = VApp (weakenValue x) (weakenValue y)
weakenValue (VLam clos)  = VLam $ weakenClosure clos
weakenValue (VPi x clos) = VPi (weakenValue x) (weakenClosure clos)
weakenValue VType        = VType


weakenEnv :: Env from to -> Env from (S to)
weakenEnv Nil        = Nil
weakenEnv (Ext xs x) = Ext (weakenEnv xs) (weakenValue x)


weakenClosure :: Closure vars -> Closure (S vars)
weakenClosure (Lazily from env x) = Lazily from (weakenEnv env) x
