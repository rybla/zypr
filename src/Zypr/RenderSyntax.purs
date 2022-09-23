module Zypr.RenderSyntax where

import Data.Tuple.Nested
import Prelude
import Zypr.Location
import Zypr.Path
import Zypr.Syntax
import Zypr.SyntaxTheme
import Data.Array (concat, concatMap, intercalate)
import Data.String (joinWith)
import Data.String.CodeUnits as String
import Effect.Exception.Unsafe (unsafeThrow)
import React (ReactThis)
import React.DOM as DOM
import React.DOM.Props as Props
import Undefined (undefined)
import Zypr.EditorEffect (runEditorEffect, setLocation)
import Zypr.EditorTypes (EditorProps, EditorState)

type RenderArgs
  = { this :: ReactThis EditorProps EditorState
    , thm :: SyntaxTheme
    }

renderLocation :: RenderArgs -> Location -> Res
renderLocation args loc = renderPath args loc loc.path loc

renderPath :: RenderArgs -> Location -> Path -> (Location -> Res)
renderPath args loc = case _ of
  Top -> renderNode' args
  Zip { node, lefts, up, rights } -> \res ->
    ?a

renderNode' :: RenderArgs -> Location -> Res
renderNode' = undefined

renderNode :: RenderArgs -> Location -> Array Res -> Res
renderNode args loc@{ term: Term { node } } ress =
  [ DOM.div
      [ Props.className
          <<< intercalate " "
          <<< concat
          $ [ [ "term"
              , case node of
                  Var _ -> "term-var"
                  Lam _ -> "term-lam"
                  App _ -> "term-app"
                  Let _ -> "term-let"
              ]
            , if loc.path == Top then [ "selected" ] else []
            ]
      , Props.onClick \_event -> do
          runEditorEffect args.this do
            setLocation loc
      ] case node /\ ress of
      Var var /\ [] -> args.thm.term.var { var, id: [ DOM.text var.id ] }
      Lam lam /\ [ bnd, bod ] -> args.thm.term.lam { lam, bnd, bod }
      App app /\ [ apl, arg ] -> args.thm.term.app { app, apl, arg }
      Let let_ /\ [ bnd, imp, bod ] -> args.thm.term.let_ { let_, bnd, imp, bod }
      _ -> unsafeThrow "renderNode: malformed term"
  ]

-- renderNode args node@(Var var) [] = renderPreNode  [ DOM.text var.id ]
-- renderNode args (Lam lam) [ bnd, bod ] = ?a
-- renderNode args (App app) [ apl, arg ] = ?a
-- renderNode args (Let let_) [ bnd, imp, bod ] = ?a
-- renderNode _ _ _ = unsafeThrow "malformed term"
-- renderPreNode :: Node -> Res -> Res
-- renderPreNode node res = [ DOM.div [ Props.className "term" ] res ]
