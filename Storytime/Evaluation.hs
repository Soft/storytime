{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Storytime.Evaluation (Eval(..), toText) where

import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Storytime.Types

variable :: Env -> Name -> Int
variable e n = M.findWithDefault 0 n e

class Eval a where
  type Result a
  eval :: Env -> a -> Result a

instance Eval Value where
  type Result Value = Int
  eval _ (EInt n) = n
  eval e (EVar v) = variable e v

instance Eval BExpr where
  type Result BExpr = Bool
  eval e (Equal a b) = numBinop (==) e a b
  eval e (LessThan a b) = numBinop (<) e a b
  eval e (GreaterThan a b) = numBinop (>) e a b
  eval e (Not a) = not $ eval e a
  eval e (And a b) = eval e a && eval e b
  eval e (Or a b) = eval e a && eval e b

instance Eval Act where
  type Result Act = Env
  eval e (Assign n v) = M.insert n (eval e v) e
  eval e (Inc n) = M.insertWith (+) n 1 e
  eval e (Dec n) = M.insertWith (+) n (-1) e

instance Eval IExpr where
  type Result IExpr = Int
  eval e (Plus a b) = eval e a + eval e b
  eval e (Minus a b) = eval e a - eval e b
  eval e (Mult a b) = eval e a * eval e b
  eval e (Val v) = eval e v

instance Eval Span where
  type Result Span = T.Text
  eval _ (Lit a) = a
  eval e (Var n) = T.pack . show $ variable e n
  eval e (Cond ex a) | eval e ex = a
                     | otherwise = ""

toText :: Env -> [Span] -> T.Text
toText e sp = T.concat $ eval e <$> sp

numBinop :: (Int -> Int -> a) -> Env -> Value -> Value -> a
numBinop x e a b = eval e a `x` eval e b

