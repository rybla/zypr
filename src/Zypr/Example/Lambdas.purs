module Zypr.Example.Lambdas where

import Prelude
import Zypr.Metadata
import Zypr.Syntax
import Data.Foldable (foldr)
import Zypr.Path (Path(..))

xxx :: Path
xxx =
  ( Zip
      { lefts: [ (Term { node: (Var { id: "a", md: {} }), terms: [] }) ]
      , node: (Lam { md: { indent_bod: false } })
      , rights:
          [ ( Term
                { node: (Lam { md: { indent_bod: false } })
                , terms:
                    [ (Term { node: (Var { id: "b", md: {} }), terms: [] })
                    , (Term { node: (Var { id: "a", md: {} }), terms: [] })
                    ]
                }
            )
          ]
      , up: Top
      }
  )

-- term :: Term
-- term =
--   lam 'a'
--     $ lam 'b'
--     $ lam 'c'
--     $ lam 'd'
--     $ var 'a'
--   where
--   var :: Id -> Term
--   var id = Var { id, md: defaultVarMetadata }
--   lam :: Id -> Term -> Term
--   lam id bod =
--     Lam
--       { var: var id
--       , bod
--       , md: defaultLamMetadata
--       }
term :: Term
term = foldr ($) (var "a") [ lam "a", lam "b" ]
