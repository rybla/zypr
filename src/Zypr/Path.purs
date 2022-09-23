module Zypr.Path where

import Prelude
import Zypr.Syntax
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafeCrashWith)
import Text.PP as PP

data Path
  = Top
  | Zip
    { node :: Node -- Node of Term
    , lefts :: Array Term -- Terms to the left, reversed
    , up :: Path -- Path up
    , rights :: Array Term -- Terms to the right
    }

-- pattern matching
casePath ::
  forall a.
  { top :: Unit -> a
  , lam ::
      { bnd :: { lam :: Lam, bnd :: Path, bod :: Term } -> a
      , bod :: { lam :: Lam, bnd :: Term, bod :: Path } -> a
      }
  , app ::
      { apl :: { app :: App, apl :: Path, arg :: Term } -> a
      , arg :: { app :: App, apl :: Term, arg :: Path } -> a
      }
  , let_ ::
      { bnd :: { let_ :: Let, bnd :: Path, imp :: Term, bod :: Term } -> a
      , imp :: { let_ :: Let, bnd :: Term, imp :: Path, bod :: Term } -> a
      , bod :: { let_ :: Let, bnd :: Term, imp :: Term, bod :: Path } -> a
      }
  } ->
  Path ->
  a
casePath hdl = case _ of
  Top -> hdl.top unit
  Zip { node: Lam lam, lefts: [], up: bnd, rights: [ bod ] } -> hdl.lam.bnd { lam, bnd, bod }
  Zip { node: Lam lam, lefts: [ bnd ], up: bod, rights: [] } -> hdl.lam.bod { lam, bnd, bod }
  Zip { node: App app, lefts: [], up: apl, rights: [ arg ] } -> hdl.app.apl { app, apl, arg }
  Zip { node: App app, lefts: [ apl ], up: arg, rights: [] } -> hdl.app.arg { app, apl, arg }
  Zip { node: Let let_, lefts: [], up: bnd, rights: [ imp, bod ] } -> hdl.let_.bnd { let_, bnd, imp, bod }
  Zip { node: Let let_, lefts: [ bnd ], up: imp, rights: [ bod ] } -> hdl.let_.imp { let_, bnd, imp, bod }
  Zip { node: Let let_, lefts: [ imp, bnd ], up: bod, rights: [] } -> hdl.let_.bod { let_, bnd, imp, bod }
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
            , arg: \app -> go ((PP.paren <<< PP.words) [ doc, PP.pp app.arg ]) app.arg
            }
        , let_:
            { bnd: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", doc, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]) let_.bnd
            , imp: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", doc, PP.pp "in", PP.pp let_.bod ]) let_.imp
            , bod: \let_ -> go ((PP.paren <<< PP.words) [ PP.pp "let", PP.pp let_.bnd, PP.pp "=", PP.pp let_.imp, PP.pp "in", PP.pp let_.bod ]) let_.bod
            }
        }
