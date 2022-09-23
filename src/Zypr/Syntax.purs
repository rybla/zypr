module Zypr.Syntax where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.PP as PP
import Zypr.Metadata (AppMetadata, LamMetadata, VarMetadata, LetMetadata)

data Term
  = Term { node :: Node, terms :: Array Term }

data Node
  = Var Var
  | Lam Lam
  | App App
  | Let Let

type Var
  = { id :: Id, md :: VarMetadata }

type Lam
  = { md :: LamMetadata }

type App
  = { md :: AppMetadata }

type Let
  = { md :: LetMetadata }

type Id
  = String

-- pattern matching
caseTerm ::
  forall a.
  { var :: { var :: Var } -> a
  , lam :: { lam :: Lam, bnd :: Term, bod :: Term } -> a
  , app :: { app :: App, apl :: Term, arg :: Term } -> a
  , let_ :: { let_ :: Let, bnd :: Term, imp :: Term, bod :: Term } -> a
  } ->
  Term ->
  a
caseTerm hdl = case _ of
  Term { node: Var var, terms: [] } -> hdl.var { var }
  Term { node: Lam lam, terms: [ bnd, bod ] } -> hdl.lam { lam, bnd, bod }
  Term { node: App app, terms: [ apl, arg ] } -> hdl.app { app, apl, arg }
  Term { node: Let let_, terms: [ bnd, imp, bod ] } -> hdl.let_ { let_, bnd, imp, bod }
  term -> unsafeThrow $ "malformed term: " <> show term

-- constructors
mkVar :: { var :: Var } -> Term
mkVar { var } = Term { node: Var var, terms: [] }

var :: Id -> Term
var id = mkVar { var: { id, md: {} } }

mkLam :: { lam :: Lam, bnd :: Term, bod :: Term } -> Term
mkLam { lam, bnd, bod } = Term { node: Lam lam, terms: [ bnd, bod ] }

lam :: Id -> Term -> Term
lam id bod = mkLam { lam: { md: { indent_bod: false } }, bnd: var id, bod }

mkApp :: { app :: App, apl :: Term, arg :: Term } -> Term
mkApp { app, apl, arg } = Term { node: App app, terms: [ apl, arg ] }

app :: Term -> Term -> Term
app apl arg = mkApp { app: { md: { indent_arg: false } }, apl, arg }

mkLet :: { let_ :: Let, bnd :: Term, imp :: Term, bod :: Term } -> Term
mkLet { let_, bnd, imp } = Term { node: Let let_, terms: [ bnd, imp ] }

let_ :: Term -> Term -> Term -> Term
let_ bnd imp bod = mkLet { let_: { md: { indent_imp: false, indent_bod: false } }, bnd, imp, bod }

-- instances
derive instance genericTerm :: Generic Term _

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show x = genericShow x

instance ppTerm :: PP.PP Term where
  pp =
    caseTerm
      { var: \var -> PP.pp var.var.id
      , lam: \lam -> (PP.paren <<< PP.words) [ PP.pp "fun", PP.pp lam.bnd, PP.pp "=>", PP.pp lam.bod ]
      , app: \app -> (PP.paren <<< PP.words) [ PP.pp app.apl, PP.pp app.arg ]
      , let_: \let_ -> (PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]
      }

derive instance genericNode :: Generic Node _

derive instance eqNode :: Eq Node

instance showNode :: Show Node where
  show x = genericShow x
