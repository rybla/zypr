module Zypr.RenderEditor where

import Prelude
import Data.Array (concat, intercalate, (:))
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
import Zypr.EditorConsole (stringEditorConsoleInfo)
import Zypr.EditorEffect (EditorEffect)
import Zypr.EditorEffect as EditorEffect
import Zypr.EditorTypes (ConsoleItemType(..), EditorGiven, EditorMode(..), EditorProps, EditorState, EditorThis)
import Zypr.Example.Applications as Applications
import Zypr.Example.Lambdas as Lambdas
import Zypr.Example.YCombinator as YCombinator
import Zypr.KeyboardEventHandler (keyboardEventHandler)
import Zypr.Path (Path(..))
import Zypr.RenderSyntax (renderCursorMode, renderSelectMode, renderTopMode)
import Zypr.SyntaxTheme (Res)
import Zypr.Menu (renderMenu)

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
          , renderConsole this state
          ]

  componentDidMount = do
    Console.log "componentDidMound"
    win <- window
    listener <- eventListener (keyboardEventHandler this)
    addEventListener (EventType "keydown") listener false (toEventTarget win)

renderProgram :: EditorThis -> EditorState -> Res
renderProgram this state =
  [ DOM.div [ Props.className "program" ] case state.mode of
      TopMode top ->
        renderTopMode
          { this, thm: state.syntaxTheme }
          top
      CursorMode cursor ->
        renderCursorMode
          { this, thm: state.syntaxTheme }
          cursor
      SelectMode select ->
        renderSelectMode
          { this, thm: state.syntaxTheme }
          select
  ]

renderConsole :: EditorThis -> EditorState -> Res
renderConsole this state =
  [ DOM.div [ Props.className "console" ]
      $ intercalate [ DOM.br' ]
      $ map renderConsoleItem
      $ state.console
      <> case state.mode of
          TopMode top ->
            [ stringEditorConsoleInfo <<< intercalate "\n"
                $ [ "mode: top"
                  , "term: " <> pprint top.term
                  ]
            ]
          CursorMode cursor ->
            [ stringEditorConsoleInfo <<< intercalate "\n"
                $ [ "mode: cursor"
                  , "cursor location:"
                  , "  path: " <> pprint cursor.location.path
                  , "  term: " <> pprint cursor.location.term
                  ]
            ]
          SelectMode select ->
            [ stringEditorConsoleInfo <<< intercalate "\n"
                $ [ "mode: select"
                  , "selection start location:"
                  , "  path: " <> pprint select.locationStart.path
                  , "  term: " <> pprint select.locationStart.term
                  , "selection end location:"
                  , "  path: " <> pprint select.locationEnd.path
                  , "  term: " <> pprint select.locationEnd.term
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
