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
import Zypr.EditorTypes (CursorMode, EditorMode(..), EditorProps, EditorState, SelectMode, TopMode)

type RenderArgs
  = { this :: ReactThis EditorProps EditorState
    , thm :: SyntaxTheme
    }

-- render modes
renderTopMode :: RenderArgs -> TopMode -> Res
renderTopMode args top =
  renderLocationPath args loc
    $ renderLocationSyntax args loc
  where
  loc :: Location
  loc = { syn: TermSyntax top.term, path: Top }

renderCursorMode :: RenderArgs -> CursorMode -> Res
renderCursorMode args cursor =
  renderLocationPath args cursor.location
    $ renderCursor args
    $ renderLocationSyntax args cursor.location

renderSelectMode :: RenderArgs -> SelectMode -> Res
renderSelectMode args select =
  renderLocationPath args select.locationStart
    $ renderSelectStart args
    $ renderLocationPath args select.locationEnd
    $ renderSelectEnd args
    $ renderLocationSyntax args select.locationEnd

-- render the surrounding `Path`, and inject a `Res` at the `Top` 
renderLocationPath :: RenderArgs -> Location -> (Res -> Res)
renderLocationPath args loc = case loc.path of
  Top -> identity
  Zip { dat, lefts, up, rights } -> \res ->
    let
      loc' = unsafeFromJust $ stepUp loc
    in
      renderLocationPath args loc'
        $ renderSyntaxData args loc'
        $ concat
            [ map (renderLocationSyntax args) sbls.lefts
            , [ res ]
            , map (renderLocationSyntax args) sbls.rights
            ]
    where
    sbls = siblings loc

-- only render `Syntax` at this `Location`
renderLocationSyntax :: RenderArgs -> Location -> Res
renderLocationSyntax args loc =
  renderSyntaxData args loc
    $ map (renderLocationSyntax args) (children loc)

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

-- only render `SyntaxData` at this `Location`, using pre-rendered children 
renderSyntaxData :: RenderArgs -> Location -> Array Res -> Res
renderSyntaxData args loc@{ syn } ress =
  let
    { dat, syns } = toGenSyntax syn
  in
    [ DOM.div
        [ Props.className
            <<< intercalate " "
            <<< concat
            $ [ [ "syntax" ]
              , case dat of
                  TermData datTerm ->
                    [ "term"
                    , case datTerm of
                        VarData _ -> "term-var"
                        LamData _ -> "term-lam"
                        AppData _ -> "term-app"
                        LetData _ -> "term-let"
                    ]
                  BindData _datBind -> [ "bind" ]
              ]
        , Props.onClick \event -> do
            stopPropagation event
            runEditorEffect args.this do
              setLocation loc
        ] case dat /\ ress of
        -- var
        TermData (VarData dat) /\ [] -> args.thm.term.var { dat, id: [ DOM.text dat.id ] }
        -- lam
        TermData (LamData dat) /\ [ bnd, bod ] ->
          args.thm.term.lam
            { dat
            , bnd
            , bod
            , isAss:
                case loc.path of
                  -- apl or arg
                  Zip { dat: TermData (AppData _) } -> true
                  Top -> true
                  _ -> false
            }
        -- app
        TermData (AppData dat) /\ [ apl, arg ] ->
          args.thm.term.app
            { dat
            , apl
            , arg
            , apl_isApp:
                case syns of
                  [ TermSyntax (App _), _ ] -> true
                  _ -> false
            , isApl:
                case loc.path of
                  Zip { dat: TermData (AppData _), lefts: [], rights: [ _ ] } -> true
                  _ -> false
            }
        --- let
        TermData (LetData dat) /\ [ bnd, imp, bod ] ->
          args.thm.term.let_
            { dat
            , bnd
            , imp
            , bod
            , isAss:
                case loc.path of
                  -- apl or arg
                  Zip { dat: TermData (AppData _) } -> true
                  Top -> true
                  _ -> false
            }
        -- bind
        BindData dat /\ [] -> [ DOM.text dat.id ]
        _ ->
          unsafeThrow
            $ "renderSyntaxData: malformed term:"
            <> ("\n  dat: " <> show dat)
            <> ("\n  ress: " <> "[" <> show (length ress) <> "]")
    ]

-- args.thm.term.app { dat, apl, arg }
unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = case _ of
  Just a -> a
  Nothing -> unsafeThrow "unsafeFromJust: Nothing"
