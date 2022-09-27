module Zypr.EditorTypes where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React (ReactElement, ReactThis)
import Zypr.Location (Location)
import Zypr.Path (Path)
import Zypr.Syntax (Term)
import Zypr.SyntaxTheme (SyntaxTheme, Res)

type EditorThis
  = ReactThis EditorProps EditorState

type EditorGiven
  = { state :: EditorState, render :: Effect ReactElement, componentDidMount :: Effect Unit }

type EditorProps
  = { stateInit :: EditorState }

type EditorState
  = { mode :: EditorMode
    , syntaxTheme :: SyntaxTheme
    , clipboard :: Clipboard
    , console :: Array ConsoleItem
    , consoleVisible :: Boolean
    }

data EditorMode
  = TopMode TopMode
  | CursorMode CursorMode
  | SelectMode SelectMode

type TopMode
  = { term :: Term }

type CursorMode
  = { location :: Location }

type SelectMode
  = { locationStart :: Location, locationEnd :: Location }

type Clipboard
  = Maybe (Either Term Path)

emptyClipboard :: Clipboard
emptyClipboard = Nothing

type Console
  = Array ConsoleItem

type ConsoleItem
  = { type_ :: ConsoleItemType, res :: Res }

data ConsoleItemType
  = ConsoleItemError
  | ConsoleItemInfo
  | ConsoleItemLog
