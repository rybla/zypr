module Zypr.Example.Applications where

import Prelude
import Zypr.Syntax
import Data.Array (foldl)
import Data.Foldable (foldr)

term :: Term
term =
  app
    ( app
        ( app
            (var "a")
            (app (var "b") (var "c"))
        )
        (app (var "d") (var "e"))
    )
    (app (var "f") (var "g"))
