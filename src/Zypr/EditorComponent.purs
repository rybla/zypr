module Zypr.RenderEditor where

import Prelude
import Data.Array (concat)
import Effect (Effect)
import Effect.Console as Console
import React (ReactClass, ReactElement, ReactThis, component, getProps, getState)
import React.DOM as DOM
import React.DOM.Props as Props
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Zypr.EditorConsole (stringEditorConsoleInfo)
import Zypr.EditorTypes (ConsoleItemType(..), EditorGiven, EditorProps, EditorState)
import Zypr.KeyboardEventHandler (keyboardEventHandler)
import Zypr.RenderSyntax (renderLocation)
import Zypr.SyntaxTheme (Res)

editorClass :: ReactClass EditorProps
editorClass = component "editor" editorComponent

editorComponent :: ReactThis EditorProps EditorState -> Effect EditorGiven
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
          [ renderProgram state
          , renderConsole state
          -- , [ DOM.div [ Props.className "console" ]
          --       [ DOM.div [ Props.className "console-item" ]
          --       ]
          --   ]
          ]

  componentDidMount = do
    Console.log "componentDidMound"
    win <- window
    listener <- eventListener (keyboardEventHandler this)
    addEventListener (EventType "keydown") listener false (toEventTarget win)

renderProgram :: EditorState -> Res
renderProgram state =
  [ DOM.div [ Props.className "program" ]
      $ renderLocation state.syntaxTheme state.location
  ]

renderConsole :: EditorState -> Res
renderConsole state =
  [ DOM.div [ Props.className "console" ]
      $ concat
          ( map renderConsoleItem $ state.console
              <> [ stringEditorConsoleInfo $ "location: " <> show state.location ]
          )
  ]
  where
  renderConsoleItem { type_, res } =
    [ DOM.div
        [ Props.className $ "console-item "
            <> case type_ of
                ConsoleItemError -> "console-item-error"
                ConsoleItemInfo -> "console-item-info"
        ]
        res
    ]
