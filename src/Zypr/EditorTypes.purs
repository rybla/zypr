module Zypr.EditorTypes where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React (ReactElement, ReactThis)
import Zypr.Location (Location)
import Zypr.Path (Path)
import Zypr.Syntax (Term, Id)
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
  = { location :: Location
    , query :: Query
    }

type Query
  = { input :: QueryInput
    , mb_output :: Maybe QueryOutput
    }

type QueryInput
  = { string :: String -- query string
    , ixClasp :: Int -- selected clasp index in action
    }

type QueryOutput
  = { action :: QueryAction -- matched action
    , nClasps :: Int -- number of possible clasps in action
    , change :: Either Term Path -- change to be applied
    }

data QueryAction
  = VarQueryAction Id
  | LamQueryAction
  | LetQueryAction
  | AppQueryAction

emptyQuery :: Query
emptyQuery =
  { input: { string: "", ixClasp: 0 }
  , mb_output: Nothing
  }

type SelectMode
  = { locationStart :: Location
    , locationEnd :: Location
    }

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
