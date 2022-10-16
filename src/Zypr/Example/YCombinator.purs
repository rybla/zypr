module Zypr.Example.YCombinator where

import Prelude
import Zypr.Syntax

term :: Term
term = (Let { bnd: (Bind { id: "Y" }), bod: (Hole { dat: {} }), dat: { indent_bod: true, indent_imp: true }, imp: (Lam { bnd: (Bind { id: "f" }), bod: (App { apl: (Lam { bnd: (Bind { id: "x" }), bod: (App { apl: (Var { dat: { id: "f" } }), arg: (App { apl: (Var { dat: { id: "x" } }), arg: (Var { dat: { id: "x" } }), dat: { indent_arg: false } }), dat: { indent_arg: false } }), dat: { indent_bod: false } }), arg: (Lam { bnd: (Bind { id: "x" }), bod: (App { apl: (Var { dat: { id: "f" } }), arg: (App { apl: (Var { dat: { id: "x" } }), arg: (Var { dat: { id: "x" } }), dat: { indent_arg: false } }), dat: { indent_arg: false } }), dat: { indent_bod: false } }), dat: { indent_arg: false } }), dat: { indent_bod: false } }) })
