module Zypr.RenderSyntax where

import Prelude
import Data.Array (concat, intercalate, length, mapWithIndex, replicate)
import Data.Either (Either(..))
import Data.Foldable as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.String (null) as String
import Data.Tuple.Nested ((/\))
import Effect.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import React.SyntheticEvent as SyntheticEvent
import Text.PP (pprint)
import Zypr.EditorEffect as EditorEffect
import Zypr.EditorTypes (CursorMode, EditorMode(..), EditorState, EditorThis, Query, SelectMode, TopMode)
import Zypr.Location (Location, children, siblings, stepUp, wrapPath)
import Zypr.Path (Path(..))
import Zypr.Syntax (AppData, BindData, IfData, InfixOp(..), Syntax(..), SyntaxData(..), Term(..), TermData(..), InfixData, hole, isTerm, isTermData, toGenSyntax)
import Zypr.SyntaxTheme (Res, SyntaxTheme, tk_aplHandle)
import Zypr.UnsafeNativeEventTarget as UnsafeNativeEventTarget

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
renderTopMode args top = renderLocationSyntax args loc initialIndentationLevel
  where
  loc :: Location
  loc = { syn: TermSyntax top.term, path: Top }

renderCursorMode :: RenderArgs -> CursorMode -> Res
renderCursorMode args cursor =
  renderLocationPath args cursor.location initialIndentationLevel
    $ renderCursor args cursor

renderSelectMode :: RenderArgs -> SelectMode -> Res
renderSelectMode args select =
  -- select.pathStart: path from top to select start
  renderLocationPath args { path: select.pathStart, syn: wrapPath select.locationEnd.path select.locationEnd.syn } initialIndentationLevel \il1 ->
    renderSelectStart args
      -- select.locationEnd.path: path from select start to select end
      
      $ renderLocationPath' args select.pathStart select.locationEnd il1 \il2 ->
          renderSelectEnd args
            -- select.locationEnd.syn: syntax at select end
            
            $ renderLocationSyntax args
                { syn: select.locationEnd.syn
                , path: select.pathStart <> select.locationEnd.path
                }
                il2

initialIndentationLevel :: Int
initialIndentationLevel = 0

-- Given what node I am (SyntaxData) and what child I am (Int), how much should I be indented
indentationIncrement :: SyntaxData -> Int -> Int
indentationIncrement (TermData (VarData _)) _ = 0

indentationIncrement (TermData (LamData _)) 0 = 0

indentationIncrement (TermData (LamData _)) 1 = 1

indentationIncrement (TermData (AppData _)) 0 = 0

indentationIncrement (TermData (AppData _)) _ = 1

indentationIncrement (TermData (LetData _)) 0 = 0

indentationIncrement (TermData (LetData _)) 1 = 1

indentationIncrement (TermData (LetData _)) 2 = 0

indentationIncrement (TermData (IfData _)) _ = 1

indentationIncrement (TermData (InfixData _)) _ = 1

indentationIncrement (TermData (HoleData _)) _ = 0

indentationIncrement (BindData _) _ = 0

indentationIncrement dat i = unsafeThrow $ "impossible: SyntaxData = " <> show dat <> ", i = " <> show i

-- indentationIncrement _ _ = 1
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
          (loc' { path = pathParent <> loc'.path })
          ( mapWithIndex (\ix kres -> kres (indentationIncrement dat ix))
              $ concat
                  ( [ (\loc'' inc -> renderLocationSyntax args loc'' (indentationLevel + inc)) <$> sbls.lefts
                    , [ \inc -> kres (indentationLevel + inc) ]
                    , (\loc'' inc -> renderLocationSyntax args loc'' (indentationLevel + inc)) <$> sbls.rights
                    ]
                  )
          )
          indentationLevel
    where
    sbls = siblings loc { path = pathParent <> loc.path }

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

renderQueryOutputPath :: RenderArgs -> Path -> CursorMode -> Int -> Res
renderQueryOutputPath args path cursor il =
  [ DOM.div [ Props.className "query-output-path" ]
      $ renderLocationPath (args { interactable = false }) { syn: TermSyntax hole, path } il
      $ (\res -> [ DOM.div [ Props.className "query-output-path-term" ] res ])
      <<< renderLocationSyntax args (cursor.location { path = cursor.location.path <> path })
  ]

renderClipboardTerm :: RenderArgs -> Term -> Res
renderClipboardTerm args term =
  [ DOM.div [ Props.className "clipboard-term" ]
      $ renderLocationSyntax (args { interactable = false })
          { syn: TermSyntax term, path: Top }
          initialIndentationLevel
  ]

renderClipboardPath :: RenderArgs -> Path -> Res
renderClipboardPath args path =
  [ DOM.div [ Props.className "clipboard-path" ]
      $ renderLocationPath (args { interactable = false })
          { path, syn: TermSyntax hole }
          initialIndentationLevel
      -- $ \_ -> [ DOM.div [ Props.className "clipboard-clasp" ] [] ] -- TODO: if is apl, then needs an aplHandle
      
      $ \_ -> case path of
          -- TODO: this breaks SyntaxTheme abstraction
          Zip { dat: TermData (AppData _), lefts: [], rights: [ _ ] } -> renderClasp <> tk_aplHandle
          _ -> renderClasp
  ]
  where
  renderClasp = [ DOM.div [ Props.className "clipboard-clasp" ] [] ]

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
                        IfData _ -> "term-if"
                        HoleData _ -> "term-hole"
                        InfixData _ -> "term-infix"
                    ]
                  BindData datBind ->
                    concat
                      [ [ "bind" ]
                      , if String.null datBind.id then [ "bind-empty" ] else []
                      ]
              ]
        , Props.onMouseDown \event ->
            when args.interactable do
              stopPropagation event
              EditorEffect.runEditorEffect args.this do
                EditorEffect.setLocation loc
        , Props.onMouseMove \event -> do
            buttons <- SyntheticEvent.buttons event
            when
              ( Array.and
                  [ args.interactable
                  , buttons == toNumber 1
                  , isJust (isTerm loc.syn)
                  ]
              ) do
              EditorEffect.runEditorEffect args.this do
                EditorEffect.selectMouse loc event
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
            , bod: renderWithIndent bod indentationLevel dat.indent_bod
            , isAss:
                case loc.path of
                  -- apl or arg
                  Top -> false
                  Zip { dat: TermData (AppData _) } -> true
                  Zip { dat: TermData (InfixData _) } -> true
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
                  Zip { dat: TermData (AppData _), lefts: [], rights: [ _ ] } -> false
                  _ -> true -- otherwise, always need parens as handle to entire app
            }
        -- term-let
        TermData (LetData dat) /\ [ bnd, imp, bod ] ->
          args.thm.term.let_
            { dat
            , bnd: bnd
            , imp: renderWithIndent imp indentationLevel dat.indent_imp
            , bod: renderWithIndent bod (indentationLevel - 1) dat.indent_bod
            , isAss:
                case loc.path of
                  Zip { dat: TermData (AppData _) } -> true -- isApl or isArg
                  Zip { dat: TermData (InfixData _) } -> true -- isInfixLeft or isInfixRight
                  _ -> false
            , isApl: isAtApl loc.path
            }
        -- term-if
        TermData (IfData dat) /\ [ cnd, thn, els ] ->
          args.thm.term.if_
            { dat
            , cnd: cnd
            , thn: renderWithIndent thn indentationLevel dat.indent_thn
            , els: renderWithIndent els indentationLevel dat.indent_els
            , isAss:
                case loc.path of
                  Zip { dat: TermData (AppData _) } -> true -- isApl or isArg
                  Zip { dat: TermData (InfixData _) } -> true -- isInfixLeft or isInfixRight
                  _ -> false
            , isApl: isAtApl loc.path
            }
        -- term-hole 
        TermData (HoleData dat) /\ [] ->
          args.thm.term.hole
            { dat
            , isApl: isAtApl loc.path
            }
        -- term-infix
        TermData (InfixData dat) /\ [ left, right ] ->
          args.thm.term.infix
            { dat
            , left
            , right
            , isAss:
                -- case loc.path of
                --   Zip { dat: TermData (AppData _) } -> true -- isApl or isArg
                --   Zip { dat: TermData (InfixData _), lefts: [], rights: [ _ ] } -> true -- isInfixLeft
                --   _ -> false
                -- true -- for the sake of demo'ing assoc
                case dat.infixOp /\ loc.path of
                  Cons /\ Zip { dat: TermData (InfixData { infixOp: Cons }), lefts: [ _ ], rights: [] } -> false -- is RHS of Cons
                  Comma /\ Zip { dat: TermData (InfixData { infixOp: Comma }), lefts: [ _ ], rights: [] } -> false -- is RHS of Comma
                  _ -> true
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
