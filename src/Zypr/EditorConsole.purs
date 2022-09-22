module Zypr.EditorConsole where

import Prelude
import Data.Array as Array
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.EditorTypes (ConsoleItem, ConsoleItemType(..), EditorState)
import Zypr.SyntaxTheme (Res)

stringEditorConsoleError :: String -> ConsoleItem
stringEditorConsoleError err = { type_: ConsoleItemError, res: [ DOM.text err ] }

stringEditorConsoleInfo :: String -> ConsoleItem
stringEditorConsoleInfo err = { type_: ConsoleItemInfo, res: [ DOM.text err ] }

maxConsoleItems ∷ Int
maxConsoleItems = 3

logEditorConsole :: ConsoleItem -> EditorState -> EditorState
logEditorConsole item state =
  state
    { console =
      Array.cons item
        (Array.take maxConsoleItems state.console)
    }
