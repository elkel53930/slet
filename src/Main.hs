module Main where

import Equational
import Parse
import Text.ParserCombinators.Parsec
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import qualified Data.Map as M

import Debug.Trace

type Message = [String]

interleave :: [a] -> [a] -> [a]
interleave (a1:as1) (a2:as2) = a1 : a2 : interleave as1 as2
interleave _ _ = []

main :: IO ()
main = do
    args <- getArgs
    axioms <- readFile "axioms"
    let (msg, state) = runScript Stat{results=[],defs=M.empty} axioms
--    mapM putStrLn $ interleave (zipWith (++) (repeat "> ") $ lines axioms) msg
    if length args == 1
        then do
            scripts <- readFile $ args !! 0
            let (s_msg, _) = runScript state scripts
            mapM putStrLn $ interleave (zipWith (++) (repeat "> ") $ lines scripts) s_msg
            hFlush stdout
            return ()
        else
            loop $ state

runScript :: Stat -> String -> (Message, Stat)
runScript state axiomfile = foldl f ([],state) commands
    where
        commands = lines axiomfile
        f (msg, state) command = (msg ++ msg', state')
            where (msg', state') = execute command state

loop :: Stat -> IO()
loop state = do
    putStr "> "
    hFlush stdout
    l <- getLine
    let (msg, state') = execute l state
    mapM putStrLn msg
    loop state'

execute :: String -> Stat -> (Message, Stat)
execute cmd state = 
    case p of
        Right c -> eval c state
        Left err -> ([show err], state)
    where p = parse pCommand "command" cmd

eval :: Command -> Stat -> (Message, Stat)
eval (Subst theorem var expr) state = (msg, state {results = new})
    where
        theorem' = expand state theorem
        expr' = expand state expr
        result = substitution theorem' var expr'
        new = results state ++ [result]
        msg = ["E" ++ (show $ length $ results state) ++ " : " ++ show result]
eval (Equan p theorem) state = (msg, state {results = new})
    where
        theorem' = expand state theorem
        p' = expand state p
        result = equanimity p' theorem'
        new = results state ++ [result]
        msg = ["E" ++ (show $ length $ results state) ++ " : " ++ show result]
eval (Leibniz theorem expr var p) state = (msg, state {results = new})
    where
        theorem' = expand state theorem
        expr' = expand state expr
        p' = expand state p
        result = leibniziz theorem' expr' var p'
        new = results state ++ [result]
        msg = ["E" ++ (show $ length $ results state) ++ " : " ++ show result]
eval (Display expr) state = (msg, state)
    where
        expr' = expand state expr
        msg = [show expr']
eval (Define name expr) state = (msg, state {defs = new})
    where
        expr' = expand state expr
        new = M.insert name expr' $ defs state
        msg = [name ++ " : " ++ show expr']
eval Nop state = ([], state)
eval ShowDefs state = (msg, state)
    where
        s (name,expr) = name ++ " : " ++ show (expand state expr)
        msg = map s $ M.toList $ defs state

expand :: Stat -> Expr -> Expr
expand state e = fold expander e
    where
        expander e = case e of
            Result i   ->  results state !! i
            Named n    -> (M.!) (defs state) n
            LastResult -> last $ results state
            otherwise  -> e
