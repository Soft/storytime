{-# LANGUAGE OverloadedStrings, GADTs #-}
module Storytime.Evaluation (value, evalSpan, toText, evalExpr, evalAct) where

import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Storytime.Types

value :: Env -> Value -> Int
value _ (EInt n) = n
value e (EVar v) = M.findWithDefault 0 v e

evalSpan :: Env -> Span -> T.Text
evalSpan _ (Lit a) = a
evalSpan e (Cond ex a) | evalExpr e ex = a
                       | otherwise = ""

toText :: Env -> [Span] -> T.Text
toText e sp = T.concat $ evalSpan e <$> sp

evalExpr :: Env -> Expr -> Bool
evalExpr e (Equal a b) = numBinop (==) e a b
evalExpr e (LessThan a b) = numBinop (<) e a b
evalExpr e (GreaterThan a b) = numBinop (>) e a b
evalExpr e (Not a) = not $ evalExpr e a
evalExpr e (And a b) = evalExpr e a && evalExpr e b
evalExpr e (Or a b) = evalExpr e a && evalExpr e b

numBinop :: (Int -> Int -> a) -> Env -> Value -> Value -> a
numBinop x e a b = value e a `x` value e b

evalAct :: Env -> Act -> Env
evalAct e (Assign n v) = M.insert n (value e v) e
evalAct e (Inc n) = M.insertWith (+) n 1 e
evalAct e (Dec n) = M.insertWith (+) n (-1) e
 

