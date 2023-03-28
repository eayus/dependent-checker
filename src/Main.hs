module Main where

import Check
import Context
import Data.Fin
import Data.Nat
import Eval
import Terms
import Values
import Data.Vect (Vect(Nil))


deriving instance Show (Fin vars)
deriving instance Show (Expr vars)
deriving instance Show Const


prog :: Expr Z
prog = Const (IntLit 3)
--prog = Snd (Ano (Pair Type $ Pi Type Type) (Sigma Type (Var FZ)))
{-
prog = Let (Ano (Lam $ Var FZ) (Pi Type Type)) $
       Let (Ano (Lam $ Var $ FS FZ) (Pi (App (Var FZ) Type) (App (Var FZ) Type)))
           (Var $ FS FZ)
           -}


main :: IO ()
main = do
    case infer initial prog of
        Nothing -> putStrLn "Type error" 
        Just ty -> putStrLn $ show (norm SZ Nil prog) ++ " : " ++ show (reify SZ ty)