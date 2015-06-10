{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Storytime.Evaluation (Eval(..), Evaluator, runEval, toText) where

import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Storytime.Types

variable :: Name -> Evaluator Int
variable n = M.findWithDefault 0 n <$> ask

binOp :: (Eval a, Eval b) => (Result a -> Result b -> c) -> a -> b -> Evaluator c
binOp f a b = liftM2 f (eval a) (eval b)

type Evaluator = Reader Env

class Eval a where
  type Result a
  eval :: a -> Evaluator (Result a)

instance Eval Value where
  type Result Value = Int
  eval (EInt n) = return n
  eval (EVar v) = variable v

instance Eval BExpr where
  type Result BExpr = Bool
  eval (Equal a b) = binOp (==) a b
  eval (LessThan a b) = binOp (<) a b
  eval (GreaterThan a b) = binOp (>) a b
  eval (Not a) = not <$> eval a
  eval (And a b) = binOp (&&) a b
  eval (Or a b) = binOp (||) a b

instance Eval Act where
  type Result Act = Env
  eval (Assign n v) = eval v >>= \v' -> M.insert n v' <$> ask
  eval (Inc n) = M.insertWith (+) n 1 <$> ask
  eval (Dec n) = M.insertWith (+) n (-1) <$> ask

instance Eval IExpr where
  type Result IExpr = Int
  eval (Plus a b) = binOp (+) a b
  eval (Minus a b) = binOp (-) a b
  eval (Mult a b) = binOp (*) a b
  eval (Val v) = eval v

instance Eval Span where
  type Result Span = T.Text
  eval (Lit a) = return a
  eval (Var n) = T.pack . show <$> variable n
  eval (Cond ex a) = do
    cond <- eval ex
    return $ if cond then a else ""

runEval :: Eval a => Env -> a -> Result a
runEval e a = runReader (eval a) e

toText :: Env -> [Span] -> T.Text
toText e sp = mconcat $ map (runEval e) sp

