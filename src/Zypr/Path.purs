module Zypr.Path where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.PP as PP
import Zypr.Syntax

data Path
  = Top
  | Zip
    { dat :: SyntaxData -- SyntaxData of the Syntax this is a path to
    , lefts :: Array Syntax -- Syntaxes to the left, reversed
    , up :: Path -- Path up
    , rights :: Array Syntax -- Syntaxes to the right
    }

-- append the first path above the second path
appendPaths :: Path -> Path -> Path
appendPaths path1 path2 = case path2 of
  Top -> path1
  Zip { dat, lefts, up, rights } ->
    Zip
      { dat, lefts, up: appendPaths path1 up, rights }

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
  path -> unsafeThrow $ "malformed path: " <> show path

-- instances
derive instance genericPath :: Generic Path _

derive instance eqPath :: Eq Path

instance showPath :: Show Path where
  show x = genericShow x

-- instance ppPath :: PP.PP Path where
--   pp = go (PP.pp "@")
--     where
--     go :: PP.Doc -> Path -> PP.Doc
--     go doc =
--       casePath
--         { top: \_ -> doc
--         , lam:
--             { bnd: \lam -> go ((PP.paren <<< PP.words) [ PP.pp "fun", doc, PP.pp "=>", PP.pp lam.bod ]) lam.bnd
--             , bod: \lam -> go ((PP.paren <<< PP.words) [ PP.pp "fun", PP.pp lam.bnd, PP.pp "=>", doc ]) lam.bod
--             }
--         , app:
--             { apl: \app -> go ((PP.paren <<< PP.words) [ doc, PP.pp app.arg ]) app.apl
--             , arg: \app -> go ((PP.paren <<< PP.words) [ doc, PP.pp app.arg ]) app.arg
--             }
--         , let_:
--             { bnd: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", doc, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]) let_.bnd
--             , imp: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", doc, PP.pp "in", PP.pp let_.bod ]) let_.imp
--             , bod: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]) let_.bod
--             }
--         }
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
