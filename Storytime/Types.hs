{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Storytime.Types where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type Name = T.Text

data BExpr = Or BExpr BExpr
          | And BExpr BExpr
          | Not BExpr
          | Equal Value Value
          | LessThan Value Value
          | GreaterThan Value Value
          deriving (Show, Eq)

data Value = EInt Int | EVar Name
          deriving (Show, Eq)

data Act = Inc Name
         | Dec Name
         | Assign Name IExpr
         deriving (Show, Eq)

data IExpr = Plus IExpr IExpr
           | Minus IExpr IExpr
           | Mult IExpr IExpr
           | Val Value
           deriving (Show, Eq)

type Meta = M.Map T.Text T.Text

type Tag = T.Text

data Link = Link { target :: Tag
                 , title :: T.Text
                 , cond :: Maybe BExpr
                 , acts :: [Act] }
          deriving (Show, Eq)

data Span = Lit T.Text
          | Var Name
          | Cond BExpr T.Text
          deriving (Show, Eq)

type DynText = [Span]

data Section = Sect { tag :: Tag
                    , sectActs :: [Act]
                    , spans :: DynText
                    , links :: [Link] }
             deriving (Show, Eq)

data Story = Story { meta :: Meta
                   , start :: Section
                   , sects :: M.Map Tag Section }
           deriving (Show, Eq)

type Env = M.Map Name Int

data StoryState = StoryState { env :: Env
                             , section :: Section }

data StoryError = MissingSection Tag

instance Show StoryError where
  show (MissingSection t) = "Missing section: " ++ T.unpack t

newtype Storytime m a = Storytime (ExceptT StoryError (ReaderT Story (StateT StoryState m)) a)
                      deriving ( Functor
                               , Applicative
                               , Monad
                               , MonadReader Story
                               , MonadState StoryState
                               , MonadError StoryError)

deriving instance MonadIO m => MonadIO (Storytime m)
