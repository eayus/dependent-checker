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
    CannotApply :: Expr vars -> Expr globals -> TypeError
    LamNotPi    :: Expr vars -> Expr globals -> TypeError
    Mismatch    :: Expr vars -> Expr vars -> TypeError

deriving instance Show TypeError


-- Standard bi-directional type checker for the language.
-- Note the slightly unconventional naming - the counterpart to 'infer' is
-- named 'ensure'.


infer :: Context vars globals -> Expr vars -> Either TypeError (Type globals)
infer ctx (Var v)      = pure $ llookup v ctx
infer ctx (App x y)    = inferApp ctx x y
infer ctx (Let x ty y) = inferLet ctx x ty y
infer ctx (Pi x y)     = checkPi ctx x y $> VType
infer ctx Type         = pure VType
infer ctx x            = Left $ CannotInfer x


inferApp :: Context vars globals -> Expr vars -> Expr vars -> Either TypeError (Type globals)
inferApp ctx x y = infer ctx x >>= \case
    VPi from to -> do
        ensure ctx y from
        let yv = toValue ctx y
        pure $ force to yv
    ty -> Left $ CannotApply x (reify (globals ctx) ty)


inferLet :: Context vars globals -> Expr vars -> Expr vars -> Expr (S vars) -> Either TypeError (Type globals)
inferLet ctx arg ty body = do
    let tyv = toValue ctx ty
    let argv = toValue ctx arg
    ensure ctx arg tyv
    infer (Local ctx tyv argv) body


ensure :: Context vars globals -> Expr vars -> Type globals -> Either TypeError ()
ensure ctx (Lam x) ty = ensureLam ctx x ty
ensure ctx x expected = infer ctx x >>= ensureConvertible ctx expected


ensureLam :: Context vars globals -> Expr (S vars) -> Type globals -> Either TypeError ()
ensureLam ctx body (VPi from to) = ensure (Global ctx from) body (forceFresh (globals ctx) to)
ensureLam ctx body ty            = Left $ LamNotPi body (reify (globals ctx) ty)


ensureConvertible :: Context vars globals -> Type globals -> Type globals -> Either TypeError ()
ensureConvertible ctx expected actual = do
    let expected' = reify (globals ctx) expected
    let actual' = reify (globals ctx) actual
    if expected' == actual'
        then pure ()
        else Left $ Mismatch expected' actual'


checkPi :: Context vars globals -> Expr vars -> Expr (S vars) -> Either TypeError ()
checkPi ctx from to = do
    ensure ctx from VType
    let fromv = toValue ctx from
    ensure (Global ctx fromv) to VType
