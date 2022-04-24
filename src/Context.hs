module Context (
    Context(..),
    vars,
    globals,
    llookup,
    toValue
) where

import Util
import Terms
import Values
import Weaken
import Eval


-- Type checking contexts keep track of the types in scope.
--    'vars' is how many variables are in the context
--    'globals' is how many 'global' variables are in the context
--
-- We consider 'global' variables to be those that we might want to
-- reference in a normalised type. Stepping under a lambda or a pi
-- type introduces a 'global', since the normalised type might need
-- to refer to that unknown variable. On the other hand, the variable
-- introduced by a let expression would not be global, since a
-- normalised type would substitute the value bound by the let, rather
-- than refer to it by 'name'.

data Context (vars :: Nat) (globals :: Nat) where
    Empty  :: Context Z Z
    Global :: Context vars globals -> Value globals -> Context (S vars) (S globals)
    Local  :: Context vars globals -> Value globals -> Value globals -> Context (S vars) globals


-- To avoid passing around the singletons for the 'vars' and 'globals'
-- everywhere, we can calculate them from the context. This is not the
-- most efficient, but keeps the code cleaner. A more optimal way to
-- handle this is cache these values as a part of the context.

vars :: Context vars globals -> SNat vars
vars Empty           = SZ
vars (Global ctx _)  = SS (vars ctx)
vars (Local ctx _ _) = SS (vars ctx)


globals :: Context vars globals -> SNat globals
globals Empty           = SZ
globals (Global ctx _)  = SS (globals ctx)
globals (Local ctx _ _) = globals ctx


-- Index into the context from the right, such that at index 0 is the
-- last entry in the context. This is equivalent to de Bruijn indexing.

rlookup :: Fin vars -> Context vars globals -> Value globals
rlookup FZ     (Global ctx ty)    = weakenValue ty
rlookup FZ     (Local ctx ty val) = ty
rlookup (FS i) (Global ctx ty)    = weakenValue $ rlookup i ctx
rlookup (FS i) (Local ctx ty val) = rlookup i ctx


-- Index into the context from the left, such that at index 0 is the
-- first entry that was added to the context. This is equivalent to
-- indexing by a de Bruijn level.

llookup :: Fin vars -> Context vars globals -> Value globals
llookup i ctx = rlookup (complement (vars ctx) i) ctx


-- Create an evaluation environment from the context.

toEnv :: SNat globals -> Context vars globals -> Env vars globals
toEnv SZ           Empty              = Nil
toEnv (SS globals) (Global ctx ty)    = Ext (weakenEnv $ toEnv globals ctx) (VVar $ largest globals)
toEnv globals      (Local ctx ty val) = Ext (toEnv globals ctx) val


-- Evaluate an expression to its value within a context.

toValue :: Context vars globals -> Expr vars -> Value globals
toValue ctx x = eval (vars ctx) (toEnv (globals ctx) ctx) x
