{-# LANGUAGE OverloadedStrings #-}
module Storytime.Evaluation (value, evalSpan, toText, evalBExpr, evalAct) where

import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Storytime.Types

value :: Env -> Value -> Int
value _ (EInt n) = n
value e (EVar v) = variable e v

variable :: Env -> Name -> Int
variable e n = M.findWithDefault 0 n e

evalSpan :: Env -> Span -> T.Text
evalSpan _ (Lit a) = a
evalSpan e (Var n) = T.pack . show $ variable e n
evalSpan e (Cond ex a) | evalBExpr e ex = a
                       | otherwise = ""

toText :: Env -> [Span] -> T.Text
toText e sp = T.concat $ evalSpan e <$> sp

evalBExpr :: Env -> BExpr -> Bool
evalBExpr e (Equal a b) = numBinop (==) e a b
evalBExpr e (LessThan a b) = numBinop (<) e a b
evalBExpr e (GreaterThan a b) = numBinop (>) e a b
evalBExpr e (Not a) = not $ evalBExpr e a
evalBExpr e (And a b) = evalBExpr e a && evalBExpr e b
evalBExpr e (Or a b) = evalBExpr e a && evalBExpr e b

numBinop :: (Int -> Int -> a) -> Env -> Value -> Value -> a
numBinop x e a b = value e a `x` value e b

evalAct :: Env -> Act -> Env
evalAct e (Assign n v) = M.insert n (evalIExpr e v) e
evalAct e (Inc n) = M.insertWith (+) n 1 e
evalAct e (Dec n) = M.insertWith (+) n (-1) e
 
evalIExpr :: Env -> IExpr -> Int
evalIExpr e (Plus a b) = evalIExpr e a + evalIExpr e b
evalIExpr e (Minus a b) = evalIExpr e a - evalIExpr e b
evalIExpr e (Mult a b) = evalIExpr e a * evalIExpr e b
evalIExpr e (Val v) = value e v
