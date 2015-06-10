{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Storytime.Types (Expr(..), Act(..), Name, Meta, Tag, Link(..), Span(..), DynText, Section(..), Story(..), Env, StoryState(..), Storytime(..), Value(..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type Name = T.Text

data Expr = Or Expr Expr
          | And Expr Expr
          | Not Expr
          | Equal Value Value
          | LessThan Value Value
          | GreaterThan Value Value
          deriving (Show, Eq)

data Value = EInt Int | EVar Name
          deriving (Show, Eq)

data Act = Inc Name
         | Dec Name
         | Assign Name Value
         deriving (Show, Eq)

type Meta = M.Map T.Text T.Text

type Tag = T.Text

data Link = Link { target :: Tag
                 , title :: T.Text
                 , cond :: Maybe Expr
                 , acts :: [Act] }
          deriving (Show, Eq)

data Span = Lit T.Text
          | Cond Expr T.Text
          deriving (Show, Eq)

type DynText = [Span]

data Section = Sect { tag :: Tag
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

newtype Storytime m a = Storytime (ReaderT Story (StateT StoryState m) a)
                      deriving ( Functor
                               , Applicative
                               , Monad
                               , MonadReader Story
                               , MonadState StoryState)

deriving instance MonadIO m => MonadIO (Storytime m)
