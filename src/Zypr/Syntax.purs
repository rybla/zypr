module Zypr.Syntax where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.PP as PP
import Zypr.Metadata (AppMetadata, LamMetadata, LetMetadata, VarMetadata, defaultAppMetadata, defaultLamMetadata, defaultLetMetadata, defaultVarMetadata)

data Term
  = Var { dat :: VarData }
  | Lam { dat :: LamData, bnd :: Term, bod :: Term }
  | App { dat :: AppData, apl :: Term, arg :: Term }
  | Let { dat :: LetData, bnd :: Term, imp :: Term, bod :: Term }

data TermData
  = VarData VarData
  | LamData LamData
  | AppData AppData
  | LetData LetData

type VarData
  = { id :: Id, md :: VarMetadata }

type LamData
  = { md :: LamMetadata }

type AppData
  = { md :: AppMetadata }

type LetData
  = { md :: LetMetadata }

type Id
  = String

-- shorthands
var :: Id -> Term
var id = Var { dat: { id, md: defaultVarMetadata } }

lam :: Id -> Term -> Term
lam id bod = Lam { dat: { md: defaultLamMetadata }, bnd: var id, bod }

app :: Term -> Term -> Term
app apl arg = App { dat: { md: defaultAppMetadata }, apl, arg }

let_ :: Id -> Term -> Term -> Term
let_ id imp bod = Let { dat: { md: defaultLetMetadata }, bnd: var id, imp, bod }

-- GenTerm
type GenTerm
  = { dat :: TermData, terms :: Array Term }

toGenTerm :: Term -> GenTerm
toGenTerm = case _ of
  Var var -> { dat: VarData var.dat, terms: [] }
  Lam lam -> { dat: LamData lam.dat, terms: [ lam.bnd, lam.bod ] }
  App app -> { dat: AppData app.dat, terms: [ app.apl, app.arg ] }
  Let let_ -> { dat: LetData let_.dat, terms: [ let_.bnd, let_.imp, let_.bod ] }

fromGenTerm :: GenTerm -> Term
fromGenTerm = case _ of
  { dat: VarData dat, terms: [] } -> Var { dat }
  { dat: LamData dat, terms: [ bnd, bod ] } -> Lam { dat, bnd, bod }
  { dat: AppData dat, terms: [ apl, arg ] } -> App { dat, apl, arg }
  { dat: LetData dat, terms: [ bnd, imp, bod ] } -> Let { dat, bnd, imp, bod }
  gterm -> unsafeThrow $ "malformed GenTerm: " <> show gterm

-- instances
derive instance genericTerm :: Generic Term _

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show x = genericShow x

instance ppTerm :: PP.PP Term where
  pp = case _ of
    Var var -> PP.pp var.dat.id
    Lam lam -> (PP.paren <<< PP.words) [ PP.pp "fun", PP.pp lam.bnd, PP.pp "=>", PP.pp lam.bod ]
    App app -> (PP.paren <<< PP.words) [ PP.pp app.apl, PP.pp app.arg ]
    Let let_ -> (PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]

derive instance genericTermData :: Generic TermData _

derive instance eqTermData :: Eq TermData

instance showTermData :: Show TermData where
  show x = genericShow x
