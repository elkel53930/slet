{-# LANGUAGE QuasiQuotes #-}
module Equational where

import Data.List -- delete, (\\)
import Data.String.Interpolate
import qualified Data.Map as M

type VarName = Char
type ExprName = String

data Expr = Eq [Expr]
          | Var VarName
          | Result Int
          | LastResult
          | Named ExprName
          | T
          | F
          | Not Expr
          | Or Expr Expr
          | And Expr Expr
          | Imp Expr Expr deriving (Eq)

data Stat = Stat{ results :: [Expr]
                , defs :: M.Map ExprName Expr
                } deriving Show

data Command = Nop
             | Subst Expr VarName Expr
             | Equan Expr Expr
             | Leibniz Expr Expr VarName Expr
             | Display Expr
             | Define ExprName Expr
             | ShowDefs deriving (Eq,Show)

instance Show Expr where
    show (Eq es) = [i|(#{concat . intersperse "=" $ map show es})|]
    show (Var var) = [var]
    show T = "T"
    show F = "F"
    show (Not e) = '~' : show e
    show (Or e1 e2) = [i|(#{e1}\\/#{e2})|]
    show (And e1 e2) = [i|(#{e1}/\\#{e2})|]
    show (Imp e1 e2) = [i|(#{e1}=.#{e2})|]
    show (Result i) = 'E' : show i
    show (Named n) = n
    show (LastResult) = "RES"

fold :: (Expr -> Expr) -> Expr -> Expr
fold f (Eq xs) = Eq $ map (fold f) xs
fold f (Not e) = Not $ fold f e
fold f (Or e1 e2) = Or (fold f e1) (fold f e2)
fold f (And e1 e2) = And (fold f e1) (fold f e2)
fold f (Imp e1 e2) = Imp (fold f e1) (fold f e2)
fold f e = f e

isEq :: Expr -> Bool
isEq (Eq xs) = True
isEq _ = False

pickUp :: Expr -> [Expr]
pickUp (Eq xs) = xs
pickUp e = [e]

flatten :: Expr -> Expr
flatten (Eq xs) = Eq $ concatMap f xs
    where
        f x = let z = flatten x in
            if isEq z then pickUp z else [x]
flatten x = x

-- P / P[x:=E]
substitution :: Expr -> VarName -> Expr -> Expr -- theorem -> var -> expr
substitution theorem var expr = flatten $ fold (\e -> if e == Var var then expr else e) theorem

-- P  P=Q / Q
equanimity :: Expr -> Expr -> Expr
equanimity p (Eq xs) = Eq $ xs \\ p'
    where p' = if isEq p then pickUp p else [p]

-- P=Q / E[x:=P] = E[x:=Q]
leibniziz :: Expr -> Expr -> VarName -> Expr -> Expr
leibniziz theorem expr var p =
    flatten . Eq $ f p ++ f q
    where
        q = equanimity p theorem
        f = pickUp . substitution expr var
