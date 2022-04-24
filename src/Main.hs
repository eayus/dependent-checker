module Main where

import Util
import Terms
import Values
import Eval
import Context
import Check


prog :: Expr Z
prog = Let (Lam $ Var FZ) (Pi Type Type) $
       Let (Lam $ Var $ FS FZ) (Pi (App (Var FZ) Type) (App (Var FZ) Type)) $
           (Var $ FS $ FZ)


main :: IO ()
main = do
    case infer Empty prog of
        Left err -> print err
        Right ty -> print $ reify SZ ty
