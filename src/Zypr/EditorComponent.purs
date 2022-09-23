module Zypr.RenderEditor where

import Prelude
import Data.Array (concat)
import Effect (Effect)
import Effect.Console as Console
import React (ReactClass, ReactElement, ReactThis, component, getProps, getState)
import React.DOM as DOM
import React.DOM.Props as Props
import Text.PP (pprint)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Zypr.EditorConsole (stringEditorConsoleInfo)
import Zypr.EditorTypes (ConsoleItemType(..), EditorGiven, EditorMode(..), EditorProps, EditorState, EditorThis)
import Zypr.KeyboardEventHandler (keyboardEventHandler)
import Zypr.RenderSyntax (renderLocationCursor, renderLocationSelect)
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
          [ renderProgram this state
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
      CursorMode cursor ->
        renderLocationCursor
          { this, thm: state.syntaxTheme }
          cursor.location
      SelectMode select ->
        renderLocationSelect
          { this, thm: state.syntaxTheme }
          select.locationStart
          select.locationEnd
  ]

renderConsole :: EditorThis -> EditorState -> Res
renderConsole this state =
  [ DOM.div [ Props.className "console" ] case state.mode of
      CursorMode cursor ->
        concat
          ( map renderConsoleItem $ state.console
              <> [ stringEditorConsoleInfo
                    $ "cursor location:"
                    <> "\n  path: "
                    <> pprint cursor.location.path
                    <> "\n  term: "
                    <> pprint cursor.location.term
                ]
          )
      SelectMode select ->
        concat
          ( map renderConsoleItem $ state.console
              <> [ stringEditorConsoleInfo
                    $ "selection start location:"
                    <> "\n  path: "
                    <> pprint select.locationStart.path
                    <> "\n  term: "
                    <> pprint select.locationStart.term
                , stringEditorConsoleInfo
                    $ "selection end location:"
                    <> "\n  path: "
                    <> pprint select.locationEnd.path
                    <> "\n  term: "
                    <> pprint select.locationEnd.term
                ]
          )
  ]
  where
  renderConsoleItem { type_, res } =
    [ DOM.div
        [ Props.className $ "console-item "
            <> case type_ of
                ConsoleItemError -> "console-item-error"
                ConsoleItemInfo -> "console-item-info"
                ConsoleItemLog -> "console-item-log"
        ]
        res
    ]
