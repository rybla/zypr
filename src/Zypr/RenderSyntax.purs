module Zypr.RenderSyntax where

import Data.Tuple.Nested
import Prelude
import Zypr.Location
import Zypr.Path
import Zypr.Syntax
import Zypr.SyntaxTheme
import Data.Array (concat, concatMap, intercalate, length, zip)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits as String
import Effect.Exception.Unsafe (unsafeThrow)
import React (ReactThis)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.EditorEffect (runEditorEffect, setLocation)
import Zypr.EditorTypes (EditorProps, EditorState)

type RenderArgs
  = { this :: ReactThis EditorProps EditorState
    , thm :: SyntaxTheme
    }

-- renders 
renderLocation :: RenderArgs -> Location -> Res
-- renderLocation args loc = case loc.path of
--   Top -> renderLocationTerm args loc
--   Zip { node, lefts, up, rights } -> renderNode args ?a ?a
renderLocation = unsafeThrow "unimplemented: renderLocation"

renderLocationPath :: RenderArgs -> Location -> (Res -> Res)
renderLocationPath args loc = case loc.path of
  Top -> identity
  Zip { node, lefts, up, rights } -> \res ->
    let
      i = length lefts

      k = renderLocationPath args (unsafeFromJust $ stepUp loc)
    in
      -- renderNode args loc
      --   $ map ?a (lefts <> [ ?a ] <> rights)
      unsafeThrow "unimplemented: renderLocationPath"

{-}
-- Renders a `Path` to this `Location` and the `Term` at this location 
renderLocationPath :: RenderArgs -> Location -> Res
renderLocationPath args loc = ?a

= case _ of
  Top -> renderLocationTerm args
  Zip { node, lefts, up, rights } -> \loc' ->
    let
      _ = ?a
    in
      renderNode args loc
        ( map ?a
            $ (lefts <> [ ?a ] <> rights) `zip` ?a
        )
-}
-- only render `Term` at this `Location`
renderLocationTerm :: RenderArgs -> Location -> Res
renderLocationTerm args loc =
  renderNode args loc
    $ map (renderLocationTerm args) (children loc)

-- only render `Node` at this `Location`, using pre-rendered children 
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
unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = case _ of
  Just a -> a
  Nothing -> unsafeThrow "unsafeFromJust: Nothing"
