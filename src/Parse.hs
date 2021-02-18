module Parse where

import Equational
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

pTrue :: Parser Expr
pTrue = do
    char 'T'
    return T

pFalse :: Parser Expr
pFalse = do
    char 'F'
    return F

pVar :: Parser Expr
pVar = do
    c <- lower
    return $ Var c

pNot :: Parser Expr
pNot = do
    char '~'
    e <- pExpr
    return $ Not e

pOr :: Parser Expr
pOr =  do
    char '('
    e1 <- pExpr
    string "\\/"
    e2 <- pExpr
    char ')'
    return $ Or e1 e2

pAnd :: Parser Expr
pAnd =  do
    char '('
    e1 <- pExpr
    string "/\\"
    e2 <- pExpr
    char ')'
    return $ And e1 e2

pImp :: Parser Expr
pImp = do
    char '('
    e1 <- pExpr
    string "=>"
    e2 <- pExpr
    char ')'
    return $ And e1 e2

pEq :: Parser Expr
pEq = do
    char '('
    e <- pExpr
    es <- many1 pSubEq
    char ')'
    return $ Eq (e:es)

pSubEq :: Parser Expr
pSubEq = do
    string "="
    e <- pExpr
    return e

pExpr :: Parser Expr
pExpr = try pResult
    <|> try pLastResult
    <|> try pNamed
    <|> try pVar
    <|> try pTrue
    <|> try pFalse
    <|> try pNot
    <|> try pOr
    <|> try pAnd
    <|> try pImp
    <|> try pEq

pResult :: Parser Expr
pResult = do
    char 'E'
    num <- many1 digit
    return $ Result $ read num

pLastResult :: Parser Expr
pLastResult = do
    string "RES"
    return LastResult

pExprName :: Parser String
pExprName = do
    char '#'
    name <- many1 (letter <|> digit <|> oneOf "_-.")
    return $ '#' : name


pNamed :: Parser Expr
pNamed = do
    name <- pExprName
    return $ Named name

pCommand :: Parser Command
pCommand = try pSubst
       <|> try pShowDefs
       <|> try pEquan
       <|> try pLeibniz
       <|> try pDisplay
       <|> try pDefine
       <|> try pNop

pSubst :: Parser Command
pSubst = do
    string "substitution"
    space
    theorem <- pExpr
    space
    var <- lower
    space
    expr <- pExpr
    return $ Subst theorem var expr

pEquan :: Parser Command
pEquan = do
    string "equanimity"
    space
    p <- pExpr
    space
    theorem <- pExpr
    return $ Equan p theorem


pLeibniz :: Parser Command
pLeibniz = do
    string "leibniz"
    space
    theorem <- pExpr
    space
    expr <- pExpr
    space
    var <- lower
    space
    p <- pExpr
    return $ Leibniz theorem expr var p

pDisplay :: Parser Command
pDisplay = do
    string "disp"
    space
    expr <- pExpr
    return $ Display expr

pDefine :: Parser Command
pDefine = do
    string "def"
    space
    name <- pExprName
    space
    expr <- pExpr
    return $ Define name expr

pNop :: Parser Command
pNop = do
    spaces
    return Nop

pShowDefs :: Parser Command
pShowDefs = do
    string "show"
    return ShowDefs