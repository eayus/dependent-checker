module Context (
    Context(..),
    vars,
    frees,
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
--    'frees' is how many 'free' variables are in the context
--
-- We consider 'free' variables to be those that we might want to
-- reference in a normalised type. Stepping under a lambda or a pi
-- type introduces a 'free', since the normalised type might need
-- to refer to that unknown variable. On the other hand, the variable
-- introduced by a let expression would be bound (not free), since a
-- normalised type would substitute the value bound by the let, rather
-- than refer to it by 'name'.

data Context (vars :: Nat) (frees :: Nat) where
    Empty  :: Context Z Z
    Free   :: Context vars frees -> Type frees -> Context (S vars) (S frees)
    Bound  :: Context vars frees -> Type frees -> Value frees -> Context (S vars) frees


-- To avoid passing around the singletons for the 'vars' and 'frees'
-- everywhere, we can calculate them from the context. This is not the
-- most efficient, but keeps the code cleaner. A more optimal way to
-- handle this is cache these values as a part of the context.

vars :: Context vars frees -> SNat vars
vars Empty           = SZ
vars (Free ctx _)    = SS (vars ctx)
vars (Bound ctx _ _) = SS (vars ctx)


frees :: Context vars frees -> SNat frees
frees Empty           = SZ
frees (Free ctx _)    = SS (frees ctx)
frees (Bound ctx _ _) = frees ctx


-- Index into the context from the right, such that at index 0 is the
-- last entry in the context. This is equivalent to de Bruijn indexing.

rlookup :: Fin vars -> Context vars frees -> Type frees
rlookup FZ     (Free ctx ty)      = weakenValue ty
rlookup FZ     (Bound ctx ty val) = ty
rlookup (FS i) (Free ctx ty)      = weakenValue $ rlookup i ctx
rlookup (FS i) (Bound ctx ty val) = rlookup i ctx


-- Index into the context from the left, such that at index 0 is the
-- first entry that was added to the context. This is equivalent to
-- indexing by a de Bruijn level.

llookup :: Fin vars -> Context vars frees -> Type frees
llookup i ctx = rlookup (complement (vars ctx) i) ctx


-- Create an evaluation environment from the context.

toEnv :: SNat frees -> Context vars frees -> Env vars frees
toEnv SZ         Empty              = Nil
toEnv (SS frees) (Free ctx ty)      = Ext (weakenEnv $ toEnv frees ctx) (VVar $ largest frees)
toEnv frees      (Bound ctx ty val) = Ext (toEnv frees ctx) val


-- Evaluate an expression to its value within a context.

toValue :: Context vars frees -> Expr vars -> Value frees
toValue ctx x = eval (vars ctx) (toEnv (frees ctx) ctx) x
