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
import Undefined (undefined)
import Zypr.EditorEffect (runEditorEffect, setLocation)

type RenderArgs
  = { this :: EditorThis
    , thm :: SyntaxTheme
    , mb_query :: Maybe Query
    , interactable :: Boolean
    }

initRenderArgs :: EditorThis -> EditorState -> RenderArgs
initRenderArgs this state =
  { this
  , thm: state.syntaxTheme
  , mb_query:
      case state.mode of
        CursorMode { query } -> Just query
        _ -> Nothing
  , interactable: true
  }

-- render modes
renderTopMode :: RenderArgs -> TopMode -> Res
renderTopMode args top = renderLocationSyntax args loc 0
  where
  loc :: Location
  loc = { syn: TermSyntax top.term, path: Top }

renderCursorMode :: RenderArgs -> CursorMode -> Res
renderCursorMode args cursor =
  renderLocationPath args cursor.location 0
    $ renderCursor args cursor

renderSelectMode :: RenderArgs -> SelectMode -> Res
renderSelectMode args select =
  -- select.pathStart: path from top to select start
  renderLocationPath args { path: select.pathStart, syn: wrapPath select.locationEnd.path select.locationEnd.syn } 0 \il1 ->
    renderSelectStart args
      -- select.locationEnd.path: path from select start to select end
      
      $ renderLocationPath' args select.pathStart select.locationEnd il1 \il2 ->
          renderSelectEnd args
            -- select.locationEnd.syn: syntax at select end
            
            $ renderLocationSyntax args
                { syn: select.locationEnd.syn
                , path:
                    appendPaths
                      select.pathStart
                      select.locationEnd.path
                }
                il2

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
renderLocationPath :: RenderArgs -> Location -> Int -> ((Int -> Res) -> Res)
renderLocationPath args loc topIndentation = case loc.path of
  Top -> \kres -> kres topIndentation
  Zip { dat, lefts, up, rights } -> \kres ->
    let
      loc' = unsafeFromJust $ stepUp loc -- TODO: replace with fromGenSyntax and up
    in
      renderLocationPath args loc' topIndentation \indentationLevel ->
        renderSyntaxData args loc'
          ( mapWithIndex (\ix kres -> kres (indentationIncrement dat ix))
              $ concat
                  ( [ (\loc inc -> renderLocationSyntax args loc (indentationLevel + inc)) <$> sbls.lefts
                    , [ \inc -> kres (indentationLevel + inc) ]
                    , (\loc inc -> renderLocationSyntax args loc (indentationLevel + inc)) <$> sbls.rights
                    ]
                  )
          )
          indentationLevel
    where
    sbls = siblings loc

renderLocationPath' :: RenderArgs -> Path -> Location -> Int -> ((Int -> Res) -> Res)
renderLocationPath' args pathParent loc topIndentation = case loc.path of
  Top -> \kres -> kres topIndentation
  Zip { dat, lefts, up, rights } -> \kres ->
    let
      loc' = unsafeFromJust $ stepUp loc -- TODO: replace with fromGenSyntax and up
    in
      renderLocationPath' args pathParent loc' topIndentation \indentationLevel ->
        renderSyntaxData args
          -- at the top of loc', render as if at pathParent; for proper parens
          ( loc'
              { path =
                case loc'.path of
                  Top -> pathParent
                  _ -> loc'.path
              }
          )
          ( mapWithIndex (\ix kres -> kres (indentationIncrement dat ix))
              $ concat
                  ( [ (\loc inc -> renderLocationSyntax args loc (indentationLevel + inc)) <$> sbls.lefts
                    , [ \inc -> kres (indentationLevel + inc) ]
                    , (\loc inc -> renderLocationSyntax args loc (indentationLevel + inc)) <$> sbls.rights
                    ]
                  )
          )
          indentationLevel
    where
    sbls = siblings loc

-- only render `Syntax` at this `Location`
renderLocationSyntax :: RenderArgs -> Location -> Int -> Res
renderLocationSyntax args loc indentationLevel =
  renderSyntaxData args loc
    ( mapWithIndex
        ( \i loc ->
            renderLocationSyntax args loc
              (indentationLevel + indentationIncrement (toGenSyntax (loc.syn)).dat i)
        )
        (children loc)
    )
    indentationLevel

renderCursor :: RenderArgs -> CursorMode -> Int -> Res
renderCursor args cursor il =
  [ DOM.div [ Props.className "cursor" ] case args.mb_query of
      Nothing -> renderLocationSyntax args cursor.location il
      Just query -> case query.mb_output of
        -- since no query output, nothing to display
        Nothing -> renderLocationSyntax args cursor.location il
        Just output ->
          [ DOM.div [ Props.className "query-output" ] case output.change of
              Left term -> renderQueryOutputTerm args term cursor il
              Right path -> renderQueryOutputPath args path cursor il
          ]
  ]

renderQueryOutputTerm :: RenderArgs -> Term -> CursorMode -> Int -> Res
renderQueryOutputTerm args term cursor il =
  [ DOM.div [ Props.className "query-output-term-new" ]
      $ renderLocationSyntax (args { interactable = false })
          { syn: TermSyntax term
          , path: cursor.location.path
          }
          il
  ]
    <> case (cursor.query.mb_output <#> _.change) /\ cursor.location.syn of
        Just (Left _term) /\ TermSyntax (Hole _hole) -> []
        _
          | not (cursor.query.input.displayOldTerm) -> []
          | otherwise ->
            [ DOM.div [ Props.className "query-output-term-old" ]
                $ renderLocationSyntax args cursor.location il
            ]

-- , DOM.div [ Props.className "query-output-term-old" ]
--     $ renderLocationSyntax args cursor.location il
-- ]
renderQueryOutputPath :: RenderArgs -> Path -> CursorMode -> Int -> Res
renderQueryOutputPath args path cursor il =
  [ DOM.div [ Props.className "query-output-path" ]
      $ renderLocationPath (args { interactable = false }) { syn: TermSyntax hole, path } il
      $ (\res -> [ DOM.div [ Props.className "query-output-path-term" ] res ])
      <<< renderLocationSyntax args cursor.location -- properly uses new indentation after wrapping by path
  ]

renderClipboardTerm :: RenderArgs -> Term -> Res
renderClipboardTerm args term =
  [ DOM.div [ Props.className "clipboard-term" ]
      $ renderLocationSyntax (args { interactable = false })
          { syn: TermSyntax term, path: Top }
          0
  ]

renderClipboardPath :: RenderArgs -> Path -> Res
renderClipboardPath args path =
  [ DOM.div [ Props.className "clipboard-path" ]
      $ renderLocationPath (args { interactable = false })
          { path, syn: TermSyntax hole }
          0
      $ \_ -> [ DOM.div [ Props.className "clipboard-clasp" ] [] ] -- TODO: suspicious
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
renderIndent indentationLevel res =
  let
    indentRes = [ DOM.br' ] <> replicate (indentationLevel + 1) (DOM.div [ Props.className "indentation" ] [ DOM.text "  " ])
  in
    indentRes <> res

renderWithIndent :: Res -> Int -> Boolean -> Res
renderWithIndent res indentationLevel indented
  | indented = renderIndent indentationLevel res
  | otherwise = res

isAtApl :: Path -> Boolean
isAtApl = case _ of
  Zip { dat: TermData (AppData _), lefts: [], rights: [ _ ] } -> true
  _ -> false

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
                        PlusData _ -> "term-plus"
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
        TermData (VarData dat) /\ [] ->
          args.thm.term.var
            { dat
            , id: [ DOM.text dat.id ]
            , isApl: isAtApl loc.path
            }
        -- term-lam
        TermData (LamData dat) /\ [ bnd, bod ] ->
          args.thm.term.lam
            { dat
            , bnd
            , bod:
                renderWithIndent bod indentationLevel dat.indent_bod
            , isAss:
                case loc.path of
                  -- apl or arg
                  Top -> false
                  Zip { dat: TermData (AppData _) } -> true
                  Zip { dat: TermData (PlusData _) } -> true
                  _ -> false
            , isLamBod:
                case loc.path of
                  Zip { dat: TermData (LamData _), lefts: [ _ ], rights: [] } -> true
                  _ -> false
            , bod_isLam:
                case syns of
                  [ _, TermSyntax (Lam _) ] -> true
                  _ -> false
            , isApl: isAtApl loc.path
            }
        -- term-app
        TermData (AppData dat) /\ [ apl, arg ] ->
          args.thm.term.app
            { dat
            , apl
            , arg: renderWithIndent arg indentationLevel dat.indent_arg
            , apl_isApp:
                case syns of
                  [ TermSyntax (App _), _ ] -> true
                  _ -> false
            , isApl: isAtApl loc.path
            , isAss:
                case loc.path of
                  Top -> false
                  Zip { dat: TermData (AppData _), lefts: [], rights: [ _ ] } -> false -- isApl
                  _ -> true
            }
        -- term-let
        TermData (LetData dat) /\ [ bnd, imp, bod ] ->
          args.thm.term.let_
            { dat
            , bnd: bnd
            , imp: renderWithIndent imp indentationLevel dat.indent_imp
            , bod:
                let
                  indentationLevel' = if indentationLevel > 0 then indentationLevel else -1
                in
                  renderWithIndent bod indentationLevel' dat.indent_bod
            , isAss:
                case loc.path of
                  Zip { dat: TermData (AppData _) } -> true -- isApl or isArg
                  Zip { dat: TermData (PlusData _) } -> true -- isPlusLeft or isPlusRight
                  _ -> false
            , isApl: isAtApl loc.path
            }
        -- term-hole 
        TermData (HoleData dat) /\ [] ->
          args.thm.term.hole
            { dat
            , isApl: isAtApl loc.path
            }
        -- term-plus
        TermData (PlusData dat) /\ [ left, right ] ->
          args.thm.term.plus
            { dat
            , left
            , right
            , isAss:
                case loc.path of
                  Zip { dat: TermData (AppData _) } -> true -- isApl or isArg
                  Zip { dat: TermData (PlusData _), lefts: [], rights: [ _ ] } -> true -- isPlusLeft
                  _ -> false
            , isApl: isAtApl loc.path
            }
        -- bind
        BindData dat /\ [] ->
          [ DOM.text
              if String.null dat.id then
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

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = case _ of
  Just a -> a
  Nothing -> unsafeThrow "unsafeFromJust: Nothing"
