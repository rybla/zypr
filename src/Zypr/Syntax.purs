module Zypr.Syntax where

import Prelude
import Zypr.Metadata
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String

data Term
  = Var Var
  | Lam Lam
  | App App
  | Let Let

derive instance genericTerm :: Generic Term _

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

instance showTerm :: Show Term where
  -- show x = genericShow x
  show = case _ of
    Var var -> String.singleton var.id
    Lam lam -> "(fun " <> show lam.var <> " => " <> show lam.bod <> ")"
    App app -> "(" <> show app.apl <> " " <> show app.arg <> ")"
    Let let_ -> "(let " <> show let_.var <> " = " <> show let_.imp <> " in " <> show let_.bod <> ")"
