module Main where

import Check
import Context
import Data.Fin
import Data.Nat
import Eval
import Terms
import Values


deriving instance Show (Fin vars)
deriving instance Show (Expr vars)


prog :: Expr Z
prog = Let (Ano (Lam $ Var FZ) (Pi Type Type)) $
       Let (Ano (Lam $ Var $ FS FZ) (Pi (App (Var FZ) Type) (App (Var FZ) Type)))
           (Var $ FS FZ)


main :: IO ()
main = do
    case infer initial prog of
        Nothing -> putStrLn "Type error" 
        Just ty -> print $ reify SZ ty