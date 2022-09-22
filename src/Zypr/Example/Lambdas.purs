module Zypr.Example.Lambdas
  ( term
  ) where

import Prelude
import Zypr.Syntax
import Zypr.Metadata

term :: Term
term =
  lam 'a'
    $ lam 'b'
    $ lam 'c'
    $ lam 'd'
    $ var 'a'
  where
  var :: Id -> Term
  var id = Var { id, md: defaultVarMetadata }

  lam :: Id -> Term -> Term
  lam id bod =
    Lam
      { var: var id
      , bod
      , md: defaultLamMetadata
      }
