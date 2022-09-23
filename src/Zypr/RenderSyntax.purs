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
renderLocationCursor :: RenderArgs -> Location -> Res
renderLocationCursor args loc =
  renderLocationPath args loc
    $ renderCursor args
    $ renderLocationTerm args loc

renderLocationSelect :: RenderArgs -> Location -> Location -> Res
renderLocationSelect args locStart locEnd =
  renderLocationPath args locStart
    $ renderSelectStart args
    $ renderLocationPath args locEnd
    $ renderSelectEnd args
    $ renderLocationTerm args locEnd

-- render the surrounding `Path`, and inject a `Res` at the `Top` 
renderLocationPath :: RenderArgs -> Location -> (Res -> Res)
renderLocationPath args loc = case loc.path of
  Top -> identity
  Zip { dat, lefts, up, rights } -> \res ->
    let
      loc' = unsafeFromJust $ stepUp loc
    in
      renderLocationPath args loc'
        $ renderTermData args loc'
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
  renderTermData args loc
    $ map (renderLocationTerm args) (children loc)

renderCursor :: RenderArgs -> Res -> Res
renderCursor args res =
  [ DOM.div [ Props.className "select" ]
      res
  ]

renderSelectStart :: RenderArgs -> Res -> Res
renderSelectStart args res =
  [ DOM.div [ Props.className "selectStart" ]
      res
  ]

renderSelectEnd :: RenderArgs -> Res -> Res
renderSelectEnd args res =
  [ DOM.div [ Props.className "selectEnd" ]
      res
  ]

-- only render `TermData` at this `Location`, using pre-rendered children 
renderTermData :: RenderArgs -> Location -> Array Res -> Res
renderTermData args loc@{ term } ress =
  let
    { dat } = toGenTerm term
  in
    [ DOM.div
        [ Props.className
            <<< intercalate " "
            <<< concat
            $ [ [ "term"
                , case dat of
                    VarData _ -> "term-var"
                    LamData _ -> "term-lam"
                    AppData _ -> "term-app"
                    LetData _ -> "term-let"
                ]
              ]
        , Props.onClick \event -> do
            stopPropagation event
            runEditorEffect args.this do
              setLocation loc
        ] case dat /\ ress of
        VarData dat /\ [] -> args.thm.term.var { dat, id: [ DOM.text dat.id ] }
        LamData dat /\ [ bnd, bod ] -> args.thm.term.lam { dat, bnd, bod }
        AppData dat /\ [ apl, arg ] -> args.thm.term.app { dat, apl, arg }
        LetData dat /\ [ bnd, imp, bod ] -> args.thm.term.let_ { dat, bnd, imp, bod }
        _ ->
          unsafeThrow
            $ "renderTermData: malformed term:"
            <> ("\n  dat: " <> show dat)
            <> ("\n  ress: " <> "[" <> show (length ress) <> "]")
    ]

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = case _ of
  Just a -> a
  Nothing -> unsafeThrow "unsafeFromJust: Nothing"
