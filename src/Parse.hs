{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse where

import Terms
import qualified Text.Megaparsec as M
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as M
import Control.Monad (void)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Control.Monad.Reader
import Data.Vect hiding ((++))
import Data.Fin
import Data.Nat
import Control.Monad.Morph

deriving instance Show a => Show (Vect vars a)


findIndex :: Eq a => a -> Vect len a -> Maybe (Fin len)
findIndex e Nil         = Nothing
findIndex e (Cons x xs)
    | e == x    = Just FZ
    | otherwise = FS <$> findIndex e xs


type Name = String


type Parser vars = ReaderT (Vect vars Name) (M.Parsec Void String) 


extend :: Name -> Parser (S vars) a -> Parser vars a
extend name cont = ReaderT $ runReaderT cont . Cons name


sc :: Parser vars ()
sc = L.space
    M.space1
    (L.skipLineComment "--")
    (L.skipBlockComment "/-" "-/")


lexeme :: Parser vars a -> Parser vars a
lexeme = L.lexeme sc

symbol :: String -> Parser vars ()
symbol = void . L.symbol sc


parseStage :: Parser vars Stage
parseStage = lexeme $ M.choice [ M.string "const" $> Constant, M.string "rt" $> Runtime ]


parseStagedExpr :: Parser vars (Expr vars, Stage)
parseStagedExpr = M.choice [ (, Runtime) <$> parseExpr, (, Constant) <$> M.between (symbol "[") (symbol "]") parseExpr]


parseName :: Parser vars Name
parseName = lexeme $ do
    c <- M.letterChar
    cs <- M.many (M.alphaNumChar <|> M.char '_')
    pure $ c : cs


parseSubExpr :: Parser vars (Expr vars)
parseSubExpr = do
    symbol "("
    x <- parseExpr
    symbol ")"
    pure x

parseVar :: Parser vars (Expr vars)
parseVar = do
    name <- parseName
    scope <- ask
    case findLevel name scope of
        Just i -> pure (Var i)
        Nothing -> fail $ "Out of scope name " ++ show name ++ " in " ++ show scope

parseLam :: Parser vars (Expr vars)
parseLam = do
    symbol "fn"
    name <- parseName
    symbol "=>"
    body <- extend name parseExpr
    pure $ Lam body

parsePi :: Parser vars (Expr vars)
parsePi = do
    symbol "("
    name <- parseName
    symbol ":"
    (from, n) <- parseStagedExpr
    symbol ")"
    symbol "->"
    (to, m) <- extend name parseStagedExpr
    pure $ Pi from n to m

parseLet :: Parser vars (Expr vars)
parseLet = do
    symbol "let"
    name <- parseName
    an <- M.optional $ do
        symbol ":"
        parseStagedExpr
    symbol "="
    arg <- parseExpr
    symbol ";"
    body <- extend name parseExpr
    pure $ Let an arg body

parseRun :: Parser vars (Expr vars)
parseRun = do
    ups <- M.some $ symbol "^"
    let n = Prelude.length ups - 1
    Run n <$> M.between (symbol "{") (symbol "}") parseExpr

parseIf :: Parser vars (Expr vars)
parseIf = do
    symbol "if"
    b <- parseExpr
    symbol "then"
    t <- parseExpr
    symbol "else"
    f <- parseExpr
    pure $ If b t f

parseConstant :: Parser vars (Expr vars)
parseConstant = M.choice $ map (\(s, e) -> M.try (symbol s $> e)) constants

constants :: [(String, Expr vars)]
constants =
    [ ("type", Type)
    , ("int", Const Int)
    , ("unit", Const Unit)
    , ("it", Const It)
    ]

parseLit :: Parser vars (Expr vars)
parseLit = Const . IntLit <$> lexeme L.decimal

parseFix :: Parser vars (Expr vars)
parseFix = do
    symbol "fix"
    name <- parseName
    symbol "=>"
    Fix <$> extend name parseExpr
    

parseExpr''' :: Parser vars (Expr vars)
parseExpr''' = M.choice $ map M.try
    [ parseSubExpr, parseFix, parseLit, parseConstant, parseLam, parsePi, parseLet, parseVar, parseRun, parseIf ]

parseExpr'' :: Parser vars (Expr vars)
parseExpr'' = foldl1 App <$> M.some parseExpr'''

parseExpr' :: Parser vars (Expr vars)
parseExpr' = foldr1 Sub <$> M.sepBy1 parseExpr'' (symbol "-")

parseExpr :: Parser vars (Expr vars)
parseExpr = foldr1 Add <$> M.sepBy1 parseExpr' (symbol "+")
