{-# LANGUAGE OverloadedStrings #-}
module Storytime.Evaluation (evalSpan, toText, eval, evalAct) where

import Control.Applicative
import qualified Data.Set as S
import qualified Data.Text as T

import Storytime.Types

evalSpan :: Env -> Span -> T.Text
evalSpan _ (Lit a) = a
evalSpan e (Cond ex a) | eval e ex = a
                       | otherwise = ""

toText :: Env -> [Span] -> T.Text
toText e sp = T.concat $ evalSpan e <$> sp

eval :: Env -> Expr -> Bool
eval e (Var a) = S.member a e
eval e (Not a) = not $ eval e a
eval e (And a b) = eval e a && eval e b
eval e (Or a b) = eval e a || eval e b

evalAct :: Act -> Env -> Env
evalAct (Set a) e = S.insert a e
evalAct (Unset a) e = S.delete a e
evalAct (Flip a) e | S.notMember a e = S.insert a e
                   | otherwise = S.delete a e
