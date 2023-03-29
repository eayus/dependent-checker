module Main where

import Check
import Context
import Data.Fin
import Data.Nat
import Eval
import Terms
import Values
import Data.Vect (Vect(Nil))
import Text.Megaparsec
import Parse (parseExpr)
import Control.Monad.Reader


deriving instance Show (Fin vars)
deriving instance Show (Expr vars)
deriving instance Show Stage
deriving instance Show Const


main :: IO ()
main = do
    let fname = "examples/test.dc"
    contents <- readFile fname

    case runParser (runReaderT parseExpr Nil) fname contents of
        Left err -> putStrLn $ errorBundlePretty err
        Right expr -> do

            case infer initial expr of
                Nothing -> putStrLn "Type error" 
                Just (ty, n) -> putStrLn $ show (norm SZ Nil expr) ++ " : " ++ show (reify SZ ty) ++ " @ " ++ show n