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
import Debug as Debug
import Effect.Exception.Unsafe (unsafeThrow)
import React (ReactThis)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import Text.PP as PP
import Zypr.EditorEffect (runEditorEffect, setLocation)
import Zypr.EditorTypes (EditorProps, EditorState)

type RenderArgs
  = { this :: ReactThis EditorProps EditorState
    , thm :: SyntaxTheme
    }

-- renders 
renderLocation :: RenderArgs -> Location -> Res
renderLocation args loc =
  renderLocationPath args loc
    $ renderSelected args
    $ renderLocationTerm args loc

-- render the surrounding `Path`, and inject a `Res` at the `Top` 
renderLocationPath :: RenderArgs -> Location -> (Res -> Res)
renderLocationPath args loc = case loc.path of
  Top -> identity
  Zip { node, lefts, up, rights } -> \res ->
    let
      loc' = unsafeFromJust $ stepUp loc
    in
      renderLocationPath args loc'
        $ renderNode args loc'
        $ concat
            [ map (renderLocationTerm args) sbls.lefts
            , [ res ]
            , map (renderLocationTerm args) sbls.rights
            ]
    where
    sbls = siblings loc

-- only render `Term` at this `Location`
renderLocationTerm :: RenderArgs -> Location -> Res
renderLocationTerm args loc =
  renderNode args loc
    $ map (renderLocationTerm args) (children loc)

renderSelected :: RenderArgs -> Res -> Res
renderSelected args res =
  [ DOM.div [ Props.className "selected" ]
      res
  ]

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
            ]
      , Props.onClick \event -> do
          stopPropagation event
          runEditorEffect args.this do
            setLocation loc
      ] case node /\ ress of
      Var var /\ [] -> args.thm.term.var { var, id: [ DOM.text var.id ] }
      Lam lam /\ [ bnd, bod ] -> args.thm.term.lam { lam, bnd, bod }
      App app /\ [ apl, arg ] -> args.thm.term.app { app, apl, arg }
      Let let_ /\ [ bnd, imp, bod ] -> args.thm.term.let_ { let_, bnd, imp, bod }
      _ ->
        unsafeThrow
          $ "renderNode: malformed term:"
          <> ("\n  node: " <> show node)
          <> ("\n  ress: " <> "[" <> show (length ress) <> "]")
  ]

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = case _ of
  Just a -> a
  Nothing -> unsafeThrow "unsafeFromJust: Nothing"
