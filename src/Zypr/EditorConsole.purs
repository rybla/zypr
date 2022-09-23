module Zypr.EditorConsole where

import Data.Array as Array
import React.DOM as DOM
import Zypr.EditorTypes (ConsoleItem, ConsoleItemType(..), EditorState)

stringEditorConsoleError :: String -> ConsoleItem
stringEditorConsoleError err = { type_: ConsoleItemError, res: [ DOM.text err ] }

stringEditorConsoleInfo :: String -> ConsoleItem
stringEditorConsoleInfo err = { type_: ConsoleItemInfo, res: [ DOM.text err ] }

stringEditorConsoleLog :: String -> ConsoleItem
stringEditorConsoleLog log = { type_: ConsoleItemLog, res: [ DOM.text log ] }

maxConsoleItems ∷ Int
maxConsoleItems = 3

logEditorConsole :: ConsoleItem -> EditorState -> EditorState
logEditorConsole item state =
  state
    { console =
      Array.cons item
        (Array.take maxConsoleItems state.console)
    }
