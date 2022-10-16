module Zypr.Syntax where

import Prelude
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Effect.Exception.Unsafe (unsafeThrow)
import Text.PP as PP

data Syntax
  = TermSyntax Term
  | BindSyntax Bind

isTerm :: Syntax -> Maybe Term
isTerm = case _ of
  TermSyntax dat -> Just dat
  _ -> Nothing

isBind :: Syntax -> Maybe Bind
isBind = case _ of
  BindSyntax dat -> Just dat
  _ -> Nothing

data SyntaxData
  = TermData TermData
  | BindData BindData

isTermData :: SyntaxData -> Maybe TermData
isTermData = case _ of
  TermData dat -> Just dat
  _ -> Nothing

isBindData :: SyntaxData -> Maybe BindData
isBindData = case _ of
  BindData dat -> Just dat
  _ -> Nothing

data Term
  = Var { dat :: VarData }
  | Lam { dat :: LamData, bnd :: Bind, bod :: Term }
  | App { dat :: AppData, apl :: Term, arg :: Term }
  | Let { dat :: LetData, bnd :: Bind, imp :: Term, bod :: Term }
  | Hole { dat :: HoleData }
  | Infix { dat :: InfixData, left :: Term, right :: Term }

data TermData
  = VarData VarData
  | LamData LamData
  | AppData AppData
  | LetData LetData
  | HoleData HoleData
  | InfixData InfixData

type VarData
  = { id :: Id }

type LamData
  = { indent_bod :: Boolean }

type AppData
  = { indent_arg :: Boolean }

type LetData
  = { indent_imp :: Boolean, indent_bod :: Boolean }

type HoleData
  = {}

data Bind
  = Bind BindData

type BindData
  = { id :: Id }

type InfixData
  = { infixOp :: InfixOp, indent :: Boolean }

data InfixOp
  = Plus
  | Minus
  | Times
  | Divide
  | Power
  | Mod
  | Cons
  | Comma

derive instance genericInfixOp :: Generic InfixOp _

derive instance eqInfixOp :: Eq InfixOp

instance showInfixOp :: Show InfixOp where
  show x = genericShow x

type Id
  = String

idFromString :: String -> Id
idFromString x = x

-- shorthands
var :: Id -> Term
var id = Var { dat: varData id }

varData :: Id -> VarData
varData id = { id }

lam :: Id -> Term -> Term
lam id bod =
  Lam
    { dat: lamData
    , bnd: bnd id
    , bod
    }

lamData :: LamData
lamData = { indent_bod: false }

app :: Term -> Term -> Term
app apl arg =
  App
    { dat: appData
    , apl
    , arg
    }

appData :: AppData
appData = { indent_arg: false }

apps :: Term -> Array Term -> Term
apps apl args = case Array.uncons args of
  Just { head: arg, tail: args' } -> apps (app apl arg) args'
  Nothing -> apl

let_ :: Id -> Term -> Term -> Term
let_ id imp bod =
  Let
    { dat: letData
    , bnd: bnd id
    , imp
    , bod
    }

letData :: LetData
letData = { indent_imp: false, indent_bod: true }

hole :: Term
hole = Hole { dat: holeData }

holeData :: HoleData
holeData = {}

bnd :: Id -> Bind
bnd id = Bind (bindData id)

bindData :: Id -> BindData
bindData id = { id }

infixData :: InfixOp -> InfixData
infixData infixOp = { infixOp, indent: false }

-- GenSyntax
type GenSyntax
  = { dat :: SyntaxData, syns :: Array Syntax }

toGenSyntax :: Syntax -> GenSyntax
toGenSyntax = case _ of
  TermSyntax term -> case term of
    Var var -> { dat: TermData (VarData var.dat), syns: [] }
    Lam lam -> { dat: TermData (LamData lam.dat), syns: [ BindSyntax lam.bnd, TermSyntax lam.bod ] }
    App app -> { dat: TermData (AppData app.dat), syns: [ TermSyntax app.apl, TermSyntax app.arg ] }
    Let let_ -> { dat: TermData (LetData let_.dat), syns: [ BindSyntax let_.bnd, TermSyntax let_.imp, TermSyntax let_.bod ] }
    Hole hole -> { dat: TermData (HoleData hole.dat), syns: [] }
    Infix infx -> { dat: TermData (InfixData infx.dat), syns: [ TermSyntax infx.left, TermSyntax infx.right ] }
  BindSyntax (Bind dat) -> { dat: BindData dat, syns: [] }

fromGenSyntax :: GenSyntax -> Syntax
fromGenSyntax = case _ of
  { dat: TermData (VarData dat), syns: [] } -> TermSyntax $ Var { dat }
  { dat: TermData (LamData dat), syns: [ BindSyntax bnd, TermSyntax bod ] } -> TermSyntax $ Lam { dat, bnd, bod }
  { dat: TermData (AppData dat), syns: [ TermSyntax apl, TermSyntax arg ] } -> TermSyntax $ App { dat, apl, arg }
  { dat: TermData (LetData dat), syns: [ BindSyntax bnd, TermSyntax imp, TermSyntax bod ] } -> TermSyntax $ Let { dat, bnd, imp, bod }
  { dat: TermData (InfixData dat), syns: [ TermSyntax left, TermSyntax right ] } -> TermSyntax $ Infix { dat, left, right }
  { dat: BindData dat, syns: [] } -> BindSyntax $ Bind dat
  gterm -> unsafeThrow $ "malformed GenSyntax: " <> show gterm

-- instances for Syntax
derive instance genericSyntax :: Generic Syntax _

derive instance eqSyntax :: Eq Syntax

instance showSyntax :: Show Syntax where
  show x = genericShow x

instance ppSyntax :: PP.PP Syntax where
  pp = case _ of
    TermSyntax term -> PP.pp "TermSyntax[" <> PP.pp term <> PP.pp "]"
    BindSyntax bind -> PP.pp "BindSyntax[" <> PP.pp bind <> PP.pp "]"

-- instances for SyntaxData
derive instance genericSyntaxData :: Generic SyntaxData _

derive instance eqSyntaxData :: Eq SyntaxData

instance showSyntaxData :: Show SyntaxData where
  show x = genericShow x

-- instances for Term 
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
    Hole hole -> PP.pp "?"
    Infix infx -> (PP.paren <<< PP.words) [ PP.pp infx.left, PP.pp "+", PP.pp infx.right ]

-- instances for TermData
derive instance genericTermData :: Generic TermData _

derive instance eqTermData :: Eq TermData

instance showTermData :: Show TermData where
  show x = genericShow x

-- instances for Bind 
derive instance genericBind :: Generic Bind _

derive instance eqBind :: Eq Bind

instance showBind :: Show Bind where
  show x = genericShow x

instance ppBind :: PP.PP Bind where
  pp (Bind bind) =
    PP.pp
      if String.null bind.id then "~" else bind.id
