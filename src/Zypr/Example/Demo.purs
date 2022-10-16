module Zypr.Example.Demo where

import Prelude
import Zypr.Syntax

reorder_apps :: Term
reorder_apps = (App { apl: (Var { dat: { id: "f" } }), arg: (App { apl: (Var { dat: { id: "g" } }), arg: (App { apl: (Var { dat: { id: "h" } }), arg: (Var { dat: { id: "x" } }), dat: { indent_arg: false } }), dat: { indent_arg: false } }), dat: { indent_arg: false } })

reorder_lets :: Term
reorder_lets = (Let { bnd: (Bind { id: "a" }), bod: (Let { bnd: (Bind { id: "c" }), bod: (Let { bnd: (Bind { id: "b" }), bod: (Let { bnd: (Bind { id: "d" }), bod: (Let { bnd: (Bind { id: "e" }), bod: (Hole { dat: {} }), dat: { indent_bod: true, indent_imp: false }, imp: (Hole { dat: {} }) }), dat: { indent_bod: true, indent_imp: false }, imp: (Hole { dat: {} }) }), dat: { indent_bod: true, indent_imp: false }, imp: (Hole { dat: {} }) }), dat: { indent_bod: true, indent_imp: false }, imp: (Hole { dat: {} }) }), dat: { indent_bod: true, indent_imp: false }, imp: (Hole { dat: {} }) })

scratch_example :: Term
scratch_example = (App { apl: (Var { dat: { id: "whenFlagClicked" } }), arg: (App { apl: (Var { dat: { id: "repeat" } }), arg: (App { apl: (App { apl: (Var { dat: { id: "if" } }), arg: (App { apl: (App { apl: (Var { dat: { id: "eq?" } }), arg: (Var { dat: { id: "x" } }), dat: { indent_arg: false } }), arg: (Var { dat: { id: "10" } }), dat: { indent_arg: false } }), dat: { indent_arg: false } }), arg: (App { apl: (Var { dat: { id: "sayHello" } }), arg: (Var { dat: { id: "unit" } }), dat: { indent_arg: false } }), dat: { indent_arg: true } }), dat: { indent_arg: true } }), dat: { indent_arg: true } })

tylr_user_study_example :: Term
tylr_user_study_example = (App { apl: (Var { dat: { id: "f" } }), arg: (App { apl: (Var { dat: { id: "g" } }), arg: (App { apl: (App { apl: (App { apl: (Var { dat: { id: "h" } }), arg: (Infix { dat: { indent: false, infixOp: Times }, left: (Var { dat: { id: "x" } }), right: (Var { dat: { id: "x" } }) }), dat: { indent_arg: false } }), arg: (Infix { dat: { indent: false, infixOp: Minus }, left: (Infix { dat: { indent: false, infixOp: Times }, left: (Var { dat: { id: "y" } }), right: (Var { dat: { id: "z" } }) }), right: (Var { dat: { id: "y" } }) }), dat: { indent_arg: false } }), arg: (Infix { dat: { indent: false, infixOp: Minus }, left: (Infix { dat: { indent: false, infixOp: Times }, left: (Var { dat: { id: "z" } }), right: (Var { dat: { id: "y" } }) }), right: (Var { dat: { id: "z" } }) }), dat: { indent_arg: false } }), dat: { indent_arg: false } }), dat: { indent_arg: false } })

reorder_conslist :: Term
reorder_conslist = (Infix { dat: { indent: false, infixOp: Comma }, left: (Var { dat: { id: "z" } }), right: (Infix { dat: { indent: false, infixOp: Comma }, left: (Var { dat: { id: "i" } }), right: (Infix { dat: { indent: false, infixOp: Comma }, left: (Var { dat: { id: "p" } }), right: (Infix { dat: { indent: false, infixOp: Comma }, left: (Var { dat: { id: "p" } }), right: (Infix { dat: { indent: false, infixOp: Comma }, left: (Var { dat: { id: "e" } }), right: (Var { dat: { id: "r" } }) }) }) }) }) })

manipulate_args :: Term
manipulate_args = (App { apl: (App { apl: (App { apl: (Var { dat: { id: "f" } }), arg: (Var { dat: { id: "x" } }), dat: { indent_arg: false } }), arg: (App { apl: (App { apl: (Var { dat: { id: "g" } }), arg: (Var { dat: { id: "y" } }), dat: { indent_arg: false } }), arg: (Var { dat: { id: "z" } }), dat: { indent_arg: false } }), dat: { indent_arg: false } }), arg: (App { apl: (App { apl: (App { apl: (Var { dat: { id: "h" } }), arg: (Var { dat: { id: "u" } }), dat: { indent_arg: false } }), arg: (Var { dat: { id: "v" } }), dat: { indent_arg: false } }), arg: (Var { dat: { id: "w" } }), dat: { indent_arg: false } }), dat: { indent_arg: false } })

add_fun_arg :: Term
add_fun_arg = (Let { bnd: (Bind { id: "sort" }), bod: (Let { bnd: (Bind { id: "myUnsortedList" }), bod: (Let { bnd: (Bind { id: "mySortedList" }), bod: (App { apl: (Var { dat: { id: "print" } }), arg: (Var { dat: { id: "mySortedList" } }), dat: { indent_arg: false } }), dat: { indent_bod: true, indent_imp: false }, imp: (App { apl: (Var { dat: { id: "sort" } }), arg: (Var { dat: { id: "myUnsortedList" } }), dat: { indent_arg: false } }) }), dat: { indent_bod: true, indent_imp: false }, imp: (Var { dat: { id: "..." } }) }), dat: { indent_bod: true, indent_imp: false }, imp: (Var { dat: { id: "X.ultrasort" } }) })
