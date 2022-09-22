module Zypr.Syntax where

import Prelude
import Zypr.Metadata

data Term
  = Var Var
  | Lam Lam
  | App App
  | Let Let

type Var
  = { id :: Id, md :: VarMetadata }

type Lam
  = { var :: Term, bod :: Term, md :: LamMetadata }

type App
  = { apl :: Term, arg :: Term, md :: AppMetadata }

type Let
  = { var :: Term, imp :: Term, bod :: Term, md :: LetMetadata }

type Id
  = Char
