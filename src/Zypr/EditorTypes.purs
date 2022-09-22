module Zypr.EditorTypes where

import Effect (Effect)
import React (ReactElement)
import Zypr.Location (Location)
import Zypr.SyntaxTheme (SyntaxTheme)

type EditorGiven
  = { state :: EditorState, render :: Effect ReactElement }

type EditorProps
  = { stateInit :: EditorState }

type EditorState
  = { location :: Location
    , syntaxTheme :: SyntaxTheme
    }
