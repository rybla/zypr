module Zypr.RenderSyntax
  ( renderLocation
  , renderPath
  , renderTerm
  ) where

import Prelude
import Zypr.Location
import Zypr.Location
import Zypr.Metadata
import Zypr.Path
import Zypr.Syntax
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import Undefined (undefined)
import Zypr.SyntaxTheme (SyntaxTheme, Res)

renderLocation :: SyntaxTheme -> Location -> Res
renderLocation thm loc = renderPath thm loc.path loc.term

-- TODO: this is definitely wrong... need to recursively call `renderPath``
-- Renders a path as a continuation that accepts the rendering of the thing that
-- the path points to and puts that thing into the appropriate place in the
-- rendered entirety.
renderPath :: SyntaxTheme -> Path -> (Term -> Res)
renderPath thm = case _ of
  Top -> renderTerm thm -- \term -> [ DOM.div [ Props.className "selected" ] $ renderTerm thm term ]
  Lam_var lam -> \var -> thm.term.lam { var: renderTerm thm var, bod: renderTerm thm lam.bod, md: lam.md, bod_assoc: requiresAssoc lam.bod }
  Lam_bod lam -> \bod -> thm.term.lam { var: renderTerm thm lam.var, bod: renderTerm thm bod, md: lam.md, bod_assoc: requiresAssoc bod }
  App_apl app -> \apl -> thm.term.app { apl: renderTerm thm apl, arg: renderTerm thm app.arg, md: app.md, arg_assoc: requiresAssoc app.arg }
  App_arg app -> \arg -> thm.term.app { apl: renderTerm thm app.apl, arg: renderTerm thm arg, md: app.md, arg_assoc: requiresAssoc arg }
  Let_var let_ -> \var -> thm.term.let_ { var: renderTerm thm var, imp: renderTerm thm let_.imp, bod: renderTerm thm let_.bod, md: let_.md }
  Let_imp let_ -> \imp -> thm.term.let_ { var: renderTerm thm let_.var, imp: renderTerm thm imp, bod: renderTerm thm let_.bod, md: let_.md }
  Let_bod let_ -> \bod -> thm.term.let_ { var: renderTerm thm let_.var, imp: renderTerm thm let_.imp, bod: renderTerm thm bod, md: let_.md }

renderTerm :: SyntaxTheme -> Term -> Res
renderTerm thm = case _ of
  Var var -> thm.term.var { id: thm.id var.id, md: var.md }
  Lam lam -> thm.term.lam { var: renderTerm thm lam.var, bod: renderTerm thm lam.bod, md: lam.md, bod_assoc: requiresAssoc lam.bod }
  App app -> thm.term.app { apl: renderTerm thm app.apl, arg: renderTerm thm app.arg, md: app.md, arg_assoc: requiresAssoc app.arg }
  Let let_ -> thm.term.let_ { var: renderTerm thm let_.var, imp: renderTerm thm let_.imp, bod: renderTerm thm let_.bod, md: let_.md }

requiresAssoc :: Term -> Boolean
requiresAssoc = case _ of
  Var _ -> false
  Lam _ -> true
  App _ -> true
  Let _ -> true
