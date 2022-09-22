module Zypr.Syntax where

import Prelude
import Data.UUID (UUID)
import Zypr.Metadata

data Term
  = Var Var
  | Lam Lam
  | App App
  | Let Let

type Var
  = { id :: Id, md :: VarMetadata }

type Lam
  = { id :: Id, bod :: Term, md :: LamMetadata }

type App
  = { app :: Term, arg :: Term, md :: AppMetadata }

type Let
  = { id :: Id, imp :: Term, bod :: Term, md :: LetMetadata }

type Id
  = UUID
