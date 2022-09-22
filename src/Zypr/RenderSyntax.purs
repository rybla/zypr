module Zypr.RenderSyntax
  ( renderLocation
  , renderPath
  , renderTerm
  ) where

import Prelude
import Zypr.Location (Location)
import Zypr.Path (Path(..))
import Zypr.Syntax (Id, Term(..))
import Data.String (joinWith)
import Data.String.CodeUnits as String
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.SyntaxTheme (SyntaxTheme, Res)

renderLocation :: SyntaxTheme -> Location -> Res
renderLocation thm loc = renderPath thm loc.path loc.term

-- TODO: this is definitely wrong... need to recursively call `renderPath``
-- Renders a path as a continuation that accepts the rendering of the thing that
-- the path points to and puts that thing into the appropriate place in the
-- rendered entirety.
renderPath :: SyntaxTheme -> Path -> (Term -> Res)
renderPath thm = case _ of
  Top -> renderTerm true thm
  Lam_var lam -> \var -> thm.term.lam { var: renderPath thm lam.var var, bod: renderTerm false thm lam.bod, md: lam.md, bod_assoc: requiresAssoc lam.bod }
  Lam_bod lam -> \bod -> thm.term.lam { var: renderTerm false thm lam.var, bod: renderPath thm lam.bod bod, md: lam.md, bod_assoc: requiresAssoc bod }
  App_apl app -> \apl -> thm.term.app { apl: renderPath thm app.apl apl, arg: renderTerm false thm app.arg, md: app.md, arg_assoc: requiresAssoc app.arg }
  App_arg app -> \arg -> thm.term.app { apl: renderTerm false thm app.apl, arg: renderPath thm app.arg arg, md: app.md, arg_assoc: requiresAssoc arg }
  Let_var let_ -> \var -> thm.term.let_ { var: renderPath thm let_.var var, imp: renderTerm false thm let_.imp, bod: renderTerm false thm let_.bod, md: let_.md }
  Let_imp let_ -> \imp -> thm.term.let_ { var: renderTerm false thm let_.var, imp: renderPath thm let_.imp imp, bod: renderTerm false thm let_.bod, md: let_.md }
  Let_bod let_ -> \bod -> thm.term.let_ { var: renderTerm false thm let_.var, imp: renderTerm false thm let_.imp, bod: renderPath thm let_.bod bod, md: let_.md }

renderTerm :: Boolean -> SyntaxTheme -> Term -> Res
renderTerm selected thm = case _ of
  Var var -> renderNodeTerm (mkProps selected "var") $ thm.term.var { id: renderId var.id, md: var.md }
  Lam lam -> renderNodeTerm (mkProps selected "lam") $ thm.term.lam { var: renderTerm false thm lam.var, bod: renderTerm false thm lam.bod, md: lam.md, bod_assoc: requiresAssoc lam.bod }
  App app -> renderNodeTerm (mkProps selected "app") $ thm.term.app { apl: renderTerm false thm app.apl, arg: renderTerm false thm app.arg, md: app.md, arg_assoc: requiresAssoc app.arg }
  Let let_ -> renderNodeTerm (mkProps selected "let") $ thm.term.let_ { var: renderTerm false thm let_.var, imp: renderTerm false thm let_.imp, bod: renderTerm false thm let_.bod, md: let_.md }
  where
  mkProps selected className = { selected, className }

renderId :: Id -> Res
renderId id = [ DOM.div [ Props.className "term-id" ] [ DOM.text $ String.singleton id ] ]

type NodeTermProps
  = { selected :: Boolean, className :: String }

renderNodeTerm :: NodeTermProps -> Res -> Res
renderNodeTerm props =
  renderNode
    { selected: props.selected
    , className: "term term-" <> props.className
    }

defaultNodeTermProps :: String -> NodeTermProps
defaultNodeTermProps className = { selected: false, className }

type NodeProps
  = { selected :: Boolean
    , className :: String
    }

renderNode :: NodeProps -> Res -> Res
renderNode props res = [ DOM.div [ Props.className $ joinWith " " ([ "node", props.className ] <> if props.selected then [ "selected" ] else []) ] res ]

requiresAssoc :: Term -> Boolean
requiresAssoc = case _ of
  Var _ -> false
  Lam _ -> true
  App _ -> true
  Let _ -> true
