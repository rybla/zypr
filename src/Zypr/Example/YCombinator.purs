module Zypr.Example.YCombinator where

import Prelude
import Zypr.Syntax

term :: Term
term =
  let_ "y"
    ( lam "f"
        ( app
            (lam "x" (app (var "f") (app (var "x") (var "x"))))
            (lam "x" (app (var "f") (app (var "x") (var "x"))))
        )
    )
    (app (var "y") (var "y"))
