module Check where

import Util
import Terms
import Values
import Context
import Eval
import Weaken

import Data.Functor (($>))


data TypeError where
    CannotInfer :: Expr vars -> TypeError
    CannotApply :: Expr vars -> Expr frees -> TypeError
    LamNotPi    :: Expr vars -> Expr frees -> TypeError
    Mismatch    :: Expr vars -> Expr vars -> TypeError

deriving instance Show TypeError


-- Standard bi-directional type checker for the language.
-- Note the slightly unconventional naming - the counterpart to 'infer' is
-- named 'ensure'.


infer :: Context vars frees -> Expr vars -> Either TypeError (Type frees)
infer ctx (Var v)      = pure $ llookup v ctx
infer ctx (App x y)    = inferApp ctx x y
infer ctx (Let x ty y) = inferLet ctx x ty y
infer ctx (Pi x y)     = checkPi ctx x y $> VType
infer ctx Type         = pure VType
infer ctx x            = Left $ CannotInfer x


inferApp :: Context vars frees -> Expr vars -> Expr vars -> Either TypeError (Type frees)
inferApp ctx x y = infer ctx x >>= \case
    VPi from to -> do
        ensure ctx y from
        let yv = toValue ctx y
        pure $ force to yv
    ty -> Left $ CannotApply x (reify (frees ctx) ty)


inferLet :: Context vars frees -> Expr vars -> Expr vars -> Expr (S vars) -> Either TypeError (Type frees)
inferLet ctx arg ty body = do
    let tyv = toValue ctx ty
    let argv = toValue ctx arg
    ensure ctx arg tyv
    infer (Bound ctx tyv argv) body


ensure :: Context vars frees -> Expr vars -> Type frees -> Either TypeError ()
ensure ctx (Lam x) ty = ensureLam ctx x ty
ensure ctx x expected = infer ctx x >>= ensureConvertible ctx expected


ensureLam :: Context vars frees -> Expr (S vars) -> Type frees -> Either TypeError ()
ensureLam ctx body (VPi from to) = ensure (Free ctx from) body (forceFresh (frees ctx) to)
ensureLam ctx body ty            = Left $ LamNotPi body (reify (frees ctx) ty)


ensureConvertible :: Context vars frees -> Type frees -> Type frees -> Either TypeError ()
ensureConvertible ctx expected actual = do
    let expected' = reify (frees ctx) expected
    let actual' = reify (frees ctx) actual
    if expected' == actual'
        then pure ()
        else Left $ Mismatch expected' actual'


checkPi :: Context vars frees -> Expr vars -> Expr (S vars) -> Either TypeError ()
checkPi ctx from to = do
    ensure ctx from VType
    let fromv = toValue ctx from
    ensure (Free ctx fromv) to VType
