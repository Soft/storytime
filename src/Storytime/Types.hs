{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Storytime.Types where

import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.UUID (UUID)

type Env = M.Map Name Integer

data UserState = UserState
  { env :: Env
  , section :: Section
  }

type UserID = UUID

data StoryState = StoryState
  { story :: Story
  , userState :: TVar (M.Map UserID UserState)
  }

type Storytime m = ReaderT StoryState m

data MissingSection = MissingSection

data Section = Sect
  { tag :: Tag
  , sectActs :: [Act]
  , spans :: DynText
  , links :: [Link]
  } deriving (Show, Eq)

data Story = Story
  { meta :: Meta
  , start :: Section
  , sects :: M.Map Tag Section
  } deriving (Show, Eq)

type Name = T.Text

data BExpr
  = Or BExpr
       BExpr
  | And BExpr
        BExpr
  | Not BExpr
  | Equal Value
          Value
  | LessThan Value
             Value
  | GreaterThan Value
                Value
  deriving (Show, Eq)

data Value
  = EInt Integer
  | EVar Name
  deriving (Show, Eq)

data Act
  = Inc Name
  | Dec Name
  | Assign Name
           IExpr
  deriving (Show, Eq)

data IExpr
  = Plus IExpr
         IExpr
  | Minus IExpr
          IExpr
  | Mult IExpr
         IExpr
  | Val Value
  deriving (Show, Eq)

type Meta = M.Map T.Text T.Text

type Tag = T.Text

data Link = Link
  { target :: Tag
  , title :: T.Text
  , cond :: Maybe BExpr
  , acts :: [Act]
  } deriving (Show, Eq)

data Span
  = Lit T.Text
  | Var Name
  | Cond BExpr
         T.Text
  deriving (Show, Eq)

type DynText = [Span]
