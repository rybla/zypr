module Zypr.RenderEditor where

import Prelude
import Data.Array (concat, intercalate, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState)
import React.DOM as DOM
import React.DOM.Props as Props
import Text.PP (pprint)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Zypr.EditorConsole (stringEditorConsoleInfo, stringEditorConsoleLog)
import Zypr.EditorEffect (EditorEffect, runEditorEffect)
import Zypr.EditorEffect as EditorEffect
import Zypr.EditorTypes (ConsoleItemType(..), EditorGiven, EditorMode(..), EditorProps, EditorState, EditorThis)
import Zypr.Example.Applications as Applications
import Zypr.Example.Lambdas as Lambdas
import Zypr.Example.YCombinator as YCombinator
import Zypr.KeyboardEventHandler (keyboardEventHandler)
import Zypr.Menu (renderMenu)
import Zypr.Path (Path(..))
import Zypr.RenderSyntax
import Zypr.Syntax (Term)
import Zypr.SyntaxTheme (Res)

editorClass :: ReactClass EditorProps
editorClass = component "editor" editorComponent

editorComponent :: EditorThis -> Effect EditorGiven
editorComponent this = do
  props <- getProps this
  pure
    { state: props.stateInit
    , render: render <$> getState this
    , componentDidMount
    }
  where
  render :: EditorState -> ReactElement
  render state =
    DOM.div
      [ Props.className "editor" ]
      $ concat
          [ renderMenu this state
          , renderProgram this state
          , renderFooter this state
          ]

  componentDidMount = do
    Console.log "componentDidMound"
    win <- window
    listener <- eventListener (keyboardEventHandler this)
    addEventListener (EventType "keydown") listener false (toEventTarget win)

renderProgram :: EditorThis -> EditorState -> Res
renderProgram this state =
  let
    args = initRenderArgs this state
  in
    [ DOM.div [ Props.className "program" ] case state.mode of
        TopMode top -> renderTopMode args top
        CursorMode cursor -> renderCursorMode args cursor
        SelectMode select -> renderSelectMode args select
    ]

renderFooter :: EditorThis -> EditorState -> Res
renderFooter this state =
  [ DOM.div [ Props.className "footer" ]
      $ concat
          [ renderPopout this state
          , if state.consoleVisible then renderConsole this state else []
          ]
  ]

renderPopout :: EditorThis -> EditorState -> Res
renderPopout this state =
  let
    args = initRenderArgs this state
  in
    [ DOM.div [ Props.className "popout" ]
        $ concat
            [ case state.clipboard of
                Just cb ->
                  renderPopoutItem this state "clipboard"
                    [ DOM.div [ Props.className "clipboard-value" ]
                        $ case cb of
                            Left term -> renderClipboardTerm args term
                            Right path -> renderClipboardPath args path
                    ]
                Nothing -> []
            ]
    ]

renderPopoutItem :: EditorThis -> EditorState -> String -> Res -> Res
renderPopoutItem this state label res =
  [ DOM.div [ Props.className "popout-item" ]
      $ [ DOM.div [ Props.className "popout-item-label" ]
            [ DOM.text label
            , DOM.div
                [ Props.className "popout-item-clear"
                , Props.onClick \_event -> runEditorEffect this EditorEffect.clearClipboard
                ]
                [ DOM.text "✕" ]
            ]
        , DOM.br'
        , DOM.div [ Props.className "popout-item-body" ] res
        ]
  ]

{-
renderClipboard :: EditorThis -> EditorState -> Either Term Path -> Res
renderClipboard this state cb =
  let
    args = initRenderArgs this state
  in
    [ DOM.div [ Props.className "clipboard" ]
        $ [ DOM.div [ Props.className "clipboard-label" ]
              [ DOM.text "clipboard"
              , DOM.div
                  [ Props.className "clipboard-clear"
                  , Props.onClick \_event -> runEditorEffect this EditorEffect.clearClipboard
                  ]
                  [ DOM.text "✕" ]
              ]
          , DOM.br'
          , DOM.div [ Props.className "clipboard-value" ]
              $ case cb of
                  Left term -> renderClipboardTerm args term
                  Right path -> renderClipboardPath args path
          ]
    ]
-}
renderConsole :: EditorThis -> EditorState -> Res
renderConsole this state =
  [ DOM.div [ Props.className "console" ]
      $ intercalate [ DOM.br' ]
      $ map renderConsoleItem
      $ state.console
      <> case state.clipboard of
          Just (Left term) ->
            [ stringEditorConsoleInfo <<< intercalate "\n"
                $ [ "clipboard:"
                  , "term: " <> pprint term
                  ]
            ]
          Just (Right path) ->
            [ stringEditorConsoleInfo <<< intercalate "\n"
                $ [ "clipboard:"
                  , "path: " <> pprint path
                  ]
            ]
          Nothing -> []
      <> case state.mode of
          TopMode top ->
            [ stringEditorConsoleInfo <<< intercalate "\n"
                $ [ "mode: top"
                  , "term: " <> pprint top.term
                  ]
            ]
          CursorMode cursor ->
            concat
              [ case cursor.query.mb_output <#> _.change of
                  Nothing -> []
                  Just (Left term) -> [ stringEditorConsoleInfo $ "query term: " <> pprint term ]
                  Just (Right path) -> [ stringEditorConsoleInfo $ "query path: " <> pprint path ]
              , [ stringEditorConsoleInfo <<< intercalate "\n"
                    $ [ "mode: cursor"
                      , "cursor location:"
                      , "  path: " <> pprint cursor.location.path
                      , "  syn: " <> pprint cursor.location.syn
                      ]
                ]
              ]
          SelectMode select ->
            [ stringEditorConsoleInfo <<< intercalate "\n"
                $ [ "mode: select"
                  , "selection start location:"
                  , "  path: " <> pprint select.locationStart.path
                  , "  syn: " <> pprint select.locationStart.syn
                  , "selection end location:"
                  , "  path: " <> pprint select.locationEnd.path
                  , "  syn: " <> pprint select.locationEnd.syn
                  ]
            ]
  ]
  where
  renderConsoleItem item =
    [ DOM.div
        [ Props.className $ "console-item console-item-" <> typeClassName
        ]
        [ DOM.div [ Props.className $ "console-icon console-icon-" <> typeClassName ]
            [ DOM.text case item.type_ of
                ConsoleItemError -> "!"
                ConsoleItemInfo -> "&"
                ConsoleItemLog -> ">"
            ]
        , DOM.div
            [ Props.className $ "console-item-body console-item-body-" <> typeClassName
            ]
            item.res
        ]
    ]
    where
    typeClassName = case item.type_ of
      ConsoleItemError -> "error"
      ConsoleItemInfo -> "info"
      ConsoleItemLog -> "log"
