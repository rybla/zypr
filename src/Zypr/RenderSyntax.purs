module Zypr.RenderSyntax where

import Data.Tuple.Nested
import Prelude
import Zypr.EditorTypes
import Zypr.Location
import Zypr.Path
import Zypr.Syntax
import Zypr.SyntaxTheme

import Data.Array (concat, concatMap, intercalate, length, mapWithIndex, replicate, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as String
import Data.String.CodeUnits as String
import Debug as Debug
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafeCrashWith)
import React (ReactThis, getState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import Text.PP as PP
import Zypr.EditorEffect (runEditorEffect, setLocation)

type RenderArgs
  = { this :: EditorThis
    , thm :: SyntaxTheme
    , clipboard :: Clipboard
    , interactable :: Boolean
    }

initRenderArgs :: EditorThis -> EditorState -> RenderArgs
initRenderArgs this state =
  { this
  , thm: state.syntaxTheme
  , clipboard: state.clipboard
  , interactable: true
  }

-- render modes
renderTopMode :: RenderArgs -> TopMode -> Res
renderTopMode args top =
  renderLocationSyntax args loc 0
  where
  loc :: Location
  loc = { syn: TermSyntax top.term, path: Top }

renderCursorMode :: RenderArgs -> CursorMode -> Res
renderCursorMode args cursor =
  renderLocationPath args cursor.location
    $ renderCursor args
    <<< renderLocationSyntax args cursor.location

renderSelectMode :: RenderArgs -> SelectMode -> Res
renderSelectMode args select =
  renderLocationPath args select.locationStart \_il ->
    renderSelectStart args $ renderLocationPath args select.locationEnd \il ->
    renderSelectEnd args $ renderLocationSyntax args select.locationEnd il

-- Given what node I am (SyntaxData) and what child I am (Int), how much should I be indented
indentationIncrement :: SyntaxData -> Int -> Int
indentationIncrement (TermData (LamData lamData)) 1 = 1
indentationIncrement (TermData (AppData appData)) 0 = 0
indentationIncrement (TermData (AppData appData)) 1 = 1
indentationIncrement (TermData (LetData letData)) 0 = 1
indentationIncrement (TermData (LetData letData)) 1 = 1
indentationIncrement (TermData (LetData letData)) 2 = 0
indentationIncrement _ _ = 0

-- render the surrounding `Path`, and inject a `Res` at the `Top` 
renderLocationPath :: RenderArgs -> Location -> ((Int -> Res) -> Res)
renderLocationPath args loc = case loc.path of
  Top -> \kres -> kres 0
  Zip { dat, lefts, up, rights } -> \kres ->
    let
      loc' = unsafeFromJust $ stepUp loc -- note: replace with fromGenSyntax and up
    in
      renderLocationPath args loc' \ indentationLevel ->
        renderSyntaxData args loc'
        (
          mapWithIndex (\ix kres -> kres (indentationIncrement (toGenSyntax loc'.syn).dat ix))
          $ concat  
            ([ (\loc inc -> renderLocationSyntax args loc (indentationLevel + inc)) <$> sbls.lefts
            , [ \inc -> kres (indentationLevel + inc)  ]
            , (\loc inc -> renderLocationSyntax args loc (indentationLevel + inc )) <$> sbls.rights 

            ] )
        )
        indentationLevel
        -- (concat
        --     [ map (\loc -> renderLocationSyntax args loc (indentationLevel + 1)) sbls.lefts
        --     , [ kres (indentationLevel + 1) ]
        --     , map (\loc -> renderLocationSyntax args loc (indentationLevel + 1)) sbls.rights
        --     ]) indentationLevel
    where
    sbls = siblings loc

-- only render `Syntax` at this `Location`
renderLocationSyntax :: RenderArgs -> Location -> Int -> Res
renderLocationSyntax args loc indentationLevel =
  renderSyntaxData args loc
    (mapWithIndex (\ i loc
      -> renderLocationSyntax args loc
          (indentationLevel + indentationIncrement (toGenSyntax (loc.syn)).dat i))
      (children loc))
    indentationLevel

renderCursor :: RenderArgs -> Res -> Res
renderCursor args res =
  [ DOM.div [ Props.className "select" ]
      $ res
      <> case args.clipboard of
          Just (Left term) -> renderClipboardTerm args term
          Just (Right path) -> renderClipboardPath args path
          Nothing -> []
  ]

renderClipboardTerm :: RenderArgs -> Term -> Res
renderClipboardTerm args term =
  [ DOM.div [ Props.className "clipboard clipboard-term" ]
      $ renderLocationSyntax (args { interactable = false })
          { syn: TermSyntax term, path: Top } 0
  ]

renderClipboardPath :: RenderArgs -> Path -> Res
renderClipboardPath args path =
  [ DOM.div [ Props.className "clipboard clipboard-path" ]
      $ renderLocationPath (args { interactable = false })
          { path, syn: TermSyntax hole }
      $ \_ -> [ DOM.div [ Props.className "selection-hole" ] [] ] -- TODO: suspicious
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

renderIndent :: Int -> Res -> Res
renderIndent indentationLevel res
  = let indentRes = [ DOM.br' ] <> replicate indentationLevel (DOM.div [ Props.className "indentation" ] [DOM.text "  "]) in
    indentRes <> res

-- only render `SyntaxData` at this `Location`, using pre-rendered children 
renderSyntaxData :: RenderArgs -> Location -> Array Res -> Int -> Res
renderSyntaxData args loc@{ syn } ress indentationLevel =
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
                        HoleData _ -> "term-hole"
                    ]
                  BindData datBind ->
                    concat
                      [ [ "bind" ]
                      , if String.null datBind.id then [ "bind-empty" ] else []
                      ]
              ]
        , Props.onClick \event ->
            when args.interactable do
              stopPropagation event
              runEditorEffect args.this do
                setLocation loc
        ] case dat /\ ress of
        -- term-var
        TermData (VarData dat) /\ [] -> args.thm.term.var { dat, id: [ DOM.text dat.id ] }
        -- term-lam
        TermData (LamData dat) /\ [ bnd, bod ] ->
          args.thm.term.lam
            { dat
            , bnd
            , bod : if dat.indent_bod then renderIndent (indentationLevel + 1) bod else bod
            , isAss:
                case loc.path of
                  -- apl or arg
                  Zip { dat: TermData (AppData _) } -> true
                  Top -> true
                  _ -> false
            }
        -- term-app
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
        --- term-let
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
        -- term-hole 
        TermData (HoleData dat) /\ [] -> args.thm.term.hole { dat }
        -- bind
        BindData dat /\ [] ->
          [ DOM.text
              $ if String.null dat.id then
                  "~"
                else
                  dat.id
          ]
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
