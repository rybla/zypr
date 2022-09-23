module Zypr.Example.Lambdas where

import Prelude
import Zypr.Metadata
import Zypr.Syntax
import Data.Foldable (foldr)
import Zypr.Path (Path(..))

term :: Term
term = foldr ($) (var "a") [ lam "a", lam "b", lam "c", lam "d" ]
