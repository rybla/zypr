module Zypr.Example.Applications where

import Zypr.Syntax

term :: Term
term =
  apps
    (var "a")
    [ apps (var "b") [ var "c", var "d", var "e" ]
    , apps (var "f") [ var "g", var "h", var "i" ]
    , apps (var "j") [ var "k", var "l", var "m" ]
    ]
