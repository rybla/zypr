module Zypr.Path where

import Prelude
import Zypr.Syntax
import Data.Array ((:))
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.PP as PP
import Undefined (undefined)

data Path
  = Top
  | Zip
    { dat :: SyntaxData -- | SyntaxData of the Syntax this is a path to
    , lefts :: Array Syntax -- | Syntaxes to the left, reversed
    , up :: Path -- Path up
    , rights :: Array Syntax -- | Syntaxes to the right
    }

-- | append the first path above the second path
appendPath :: Path -> Path -> Path
appendPath path1 path2 = case path2 of
  Top -> path1
  Zip { dat, lefts, up, rights } ->
    Zip
      { dat, lefts, up: appendPath path1 up, rights }

instance Semigroup Path where 
  append = appendPath 

instance Monoid Path where 
  mempty = Top 

-- | path into lam with clasp at bnd
lam_bnd :: Term -> Path -> Path
lam_bnd bod up = Zip { dat: TermData (LamData lamData), lefts: [], up, rights: [ TermSyntax bod ] }

-- | path into lam with clasp at bod
lam_bod :: Bind -> Path -> Path
lam_bod bnd up = Zip { dat: TermData (LamData lamData), lefts: [ BindSyntax bnd ], up, rights: [] }

-- | path into app with clasp at apl 
app_apl :: Term -> Path -> Path
app_apl arg up = Zip { dat: TermData (AppData appData), lefts: [], up, rights: [ TermSyntax arg ] }

-- | path into app with clasp at arg
app_arg :: Term -> Path -> Path
app_arg apl up = Zip { dat: TermData (AppData appData), lefts: [ TermSyntax apl ], up, rights: [] }

-- | path into let with clasp at bnd
let_bnd :: Term -> Term -> Path -> Path
let_bnd imp bod up = Zip { dat: TermData (LetData letData), lefts: [], up, rights: [ TermSyntax imp, TermSyntax bod ] }

-- | path into let with clasp at imp
let_imp :: Bind -> Term -> Path -> Path
let_imp bnd bod up = Zip { dat: TermData (LetData letData), lefts: [ BindSyntax bnd ], up, rights: [ TermSyntax bod ] }

-- path into if with clasp at cnd
if_cnd :: Term -> Term -> Path -> Path 
if_cnd thn els up = Zip { dat: TermData (IfData ifData), lefts: [], up, rights: [TermSyntax thn, TermSyntax els  ] }

-- path into if with clasp at thn
if_thn :: Term -> Term -> Path -> Path 
if_thn cnd els up = Zip { dat: TermData (IfData ifData), lefts: [ TermSyntax cnd ], up, rights: [TermSyntax els  ] }

-- path into if with clasp at els
if_els :: Term -> Term -> Path -> Path 
if_els cnd thn up = Zip { dat: TermData (IfData ifData), lefts: [ TermSyntax thn,  TermSyntax cnd ], up, rights: [ ] }

-- | path into infix with clasp at left
infix_left :: Term -> InfixOp -> Path -> Path
infix_left right op up = Zip { dat: TermData (InfixData (infixData op)), lefts: [], up, rights: [ TermSyntax right ] }

-- | path into infix with clasp at right
infix_right :: Term -> InfixOp -> Path -> Path
infix_right left op up = Zip { dat: TermData (InfixData (infixData op)), lefts: [ TermSyntax left ], up, rights: [] }

-- | path into let with clasp at bod
let_bod :: Bind -> Term -> Path -> Path
let_bod bnd imp up = Zip { dat: TermData (LetData letData), lefts: [ TermSyntax imp, BindSyntax bnd ], up, rights: [] }

-- pattern matching
casePath ::
  forall a.
  { top :: Unit -> a
  , lam ::
      { bnd :: { dat :: LamData, bnd :: Path, bod :: Term } -> a
      , bod :: { dat :: LamData, bnd :: Bind, bod :: Path } -> a
      }
  , app ::
      { apl :: { dat :: AppData, apl :: Path, arg :: Term } -> a
      , arg :: { dat :: AppData, apl :: Term, arg :: Path } -> a
      }
  , let_ ::
      { bnd :: { dat :: LetData, bnd :: Path, imp :: Term, bod :: Term } -> a
      , imp :: { dat :: LetData, bnd :: Bind, imp :: Path, bod :: Term } -> a
      , bod :: { dat :: LetData, bnd :: Bind, imp :: Term, bod :: Path } -> a
      }
  , if_ ::
      { cnd :: { dat :: IfData, cnd :: Path, thn :: Term, els :: Term } -> a
      , thn :: { dat :: IfData, cnd :: Term, thn :: Path, els :: Term } -> a
      , els :: { dat :: IfData, cnd :: Term, thn :: Term, els :: Path } -> a
      }
  , infix ::
      { left :: { dat :: InfixData, left :: Path, right :: Term } -> a
      , right :: { dat :: InfixData, left :: Term, right :: Path } -> a
      }
  } ->
  Path ->
  a
casePath hdl = case _ of
  Top -> hdl.top unit
  Zip { dat: TermData (LamData dat), lefts: [], up: bnd, rights: [ TermSyntax bod ] } -> hdl.lam.bnd { dat, bnd, bod }
  Zip { dat: TermData (LamData dat), lefts: [ BindSyntax bnd ], up: bod, rights: [] } -> hdl.lam.bod { dat, bnd, bod }
  Zip { dat: TermData (AppData dat), lefts: [], up: apl, rights: [ TermSyntax arg ] } -> hdl.app.apl { dat, apl, arg }
  Zip { dat: TermData (AppData dat), lefts: [ TermSyntax apl ], up: arg, rights: [] } -> hdl.app.arg { dat, apl, arg }
  Zip { dat: TermData (LetData dat), lefts: [], up: bnd, rights: [ TermSyntax imp, TermSyntax bod ] } -> hdl.let_.bnd { dat, bnd, imp, bod }
  Zip { dat: TermData (LetData dat), lefts: [ BindSyntax bnd ], up: imp, rights: [ TermSyntax bod ] } -> hdl.let_.imp { dat, bnd, imp, bod }
  Zip { dat: TermData (LetData dat), lefts: [ TermSyntax imp, BindSyntax bnd ], up: bod, rights: [] } -> hdl.let_.bod { dat, bnd, imp, bod }
  Zip { dat: TermData (IfData dat), lefts: [], up: cnd, rights: [ TermSyntax thn, TermSyntax els ]  } -> hdl.if_.cnd { dat, cnd, thn, els  }
  Zip { dat: TermData (IfData dat), lefts: [ TermSyntax cnd ], up: thn, rights: [ TermSyntax els ]  } -> hdl.if_.thn { dat, cnd, thn, els  }
  Zip { dat: TermData (IfData dat), lefts: [ TermSyntax thn, TermSyntax cnd ], up: els, rights: [ ]  } -> hdl.if_.els { dat, cnd, thn, els  }
  Zip { dat: TermData (InfixData dat), lefts: [ TermSyntax left ], up: right, rights: [] } -> hdl.infix.right { dat, left, right }
  Zip { dat: TermData (InfixData dat), lefts: [], up: left, rights: [ TermSyntax right ] } -> hdl.infix.left { dat, left, right }
  path -> unsafeThrow $ "malformed path: " <> show path

-- instances
derive instance genericPath :: Generic Path _

derive instance eqPath :: Eq Path

instance showPath :: Show Path where
  show x = genericShow x

instance ppPath :: PP.PP Path where
  pp = go (PP.pp "@")
    where
    go :: PP.Doc -> Path -> PP.Doc
    go doc =
      casePath
        { top: \_ -> doc
        , lam:
            { bnd: \lam -> go ((PP.paren <<< PP.words) [ PP.pp "fun", doc, PP.pp "=>", PP.pp lam.bod ]) lam.bnd
            , bod: \lam -> go ((PP.paren <<< PP.words) [ PP.pp "fun", PP.pp lam.bnd, PP.pp "=>", doc ]) lam.bod
            }
        , app:
            { apl: \app -> go ((PP.paren <<< PP.words) [ doc, PP.pp app.arg ]) app.apl
            , arg: \app -> go ((PP.paren <<< PP.words) [ PP.pp app.apl, doc ]) app.arg
            }
        , let_:
            { bnd: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", doc, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]) let_.bnd
            , imp: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", doc, PP.pp "in", PP.pp let_.bod ]) let_.imp
            , bod: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]) let_.bod
            }
        , if_:
            { cnd: \if_ -> go ((PP.paren <<< PP.words) [ PP.pp "if", doc, PP.pp "then", PP.pp if_.thn, PP.pp "else", PP.pp if_.els ]) if_.cnd
            , thn: \if_ -> go ((PP.paren <<< PP.words) [ PP.pp "if", PP.pp if_.thn, PP.pp "then", doc, PP.pp "else", PP.pp if_.els ]) if_.thn
            , els: \if_ -> go ((PP.paren <<< PP.words) [ PP.pp "if", PP.pp if_.thn, PP.pp "then", PP.pp if_.thn, PP.pp "else", doc ]) if_.els
            }
        , infix: { left: \_ -> PP.pp "+", right: \_ -> PP.pp "+" }
        }

{-
-- useful for debugging
instance ppPath :: PP.PP Path where
  pp = case _ of
    Top -> PP.pp "Top"
    Zip { dat, lefts, up, rights } ->
      PP.words
        [ PP.pp "Zip["
        , PP.pp $ "dat = "
            <> case dat of
                TermData (VarData varDat) -> "<var: " <> varDat.id <> ">"
                TermData (LamData _) -> "<lam>"
                TermData (AppData _) -> "<app>"
                TermData (LetData _) -> "<let>"
                TermData (HoleData _) -> "<hole>"
                BindData bndDat -> "<bind: " <> bndDat.id <> ">"
        , PP.pp ","
        , PP.pp "lefts = " <> PP.ppArray lefts
        , PP.pp ","
        , PP.pp "up = " <> PP.pp up
        , PP.pp ","
        , PP.pp "rights = " <> PP.ppArray rights
        , PP.pp "]"
        ]
-}
{-
-- TODO: just use diffPath

-- checks if p1 is a parent of p2 i.e. the clasp of p1 is a parent of the clasp
-- of p2
isAbovePath :: Path -> Path -> Boolean
isAbovePath p1 p2 = undefined

-- checks if p1 is a parent of p2 i.e. the clasp of p1 is a child of the clasp
-- of p2 
isBelowPath :: Path -> Path -> Boolean
isBelowPath p1 p2 = isAbovePath p2 p1
-}
-- given that the clasp of p1 is an ancestor of the clasp of p2, calculates the
-- path from the clasp of p1 to the clasp of p2
diffPath :: Path -> Path -> Maybe Path
diffPath p1 p2 = go p2 []
  where
  go :: Path -> Array Path -> Maybe Path
  go p ps
    | p1 == p = Just $ Array.foldr append mempty  ps

  -- dat, lefts, up, rights
  go (Zip z) ps = go z.up (Zip z { up = Top } : ps)

  go _ _ = Nothing -- not in parent/child relationship

{- DEPRECATED
appendArrayPaths :: Array Path -> Maybe Path
appendArrayPaths ps = case Array.uncons ps of
  Nothing -> Nothing
  Just { head: p, tail: [] } -> Just p
  Just { head: p, tail: ps' } -> (p <> _) <$> appendArrayPaths ps'
-}