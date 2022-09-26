module Zypr.Key where

import Prelude
import Data.Array (elem)
import Data.Foldable (and)
import Data.String as String
import Web.Event.Event (Event)

foreign import eventKey :: Event -> String

foreign import shiftKey :: Event -> Boolean

foreign import metaKey :: Event -> Boolean

foreign import ctrlKey :: Event -> Boolean

foreign import altKey :: Event -> Boolean

type Key
  = { label :: String
    , shift :: Boolean
    , meta :: Boolean
    , ctrl :: Boolean
    , alt :: Boolean
    }

fromEvent :: Event -> Key
fromEvent event =
  { label: eventKey event
  , shift: shiftKey event
  , meta: metaKey event
  , ctrl: ctrlKey event
  , alt: altKey event
  }

alphaNums :: Array String
alphaNums =
  String.split (String.Pattern " ")
    "a b c d e f g h i j k l m n o p q r s t u v w x y z"

isAlphaNum :: Key -> Boolean
isAlphaNum key =
  and
    [ not key.meta
    , not key.ctrl
    , not key.alt
    , String.toLower key.label `elem` alphaNums
    ]

defaultKey :: String -> Key
defaultKey label =
  { label
  , shift: false
  , meta: false
  , ctrl: false
  , alt: false
  }

key_ArrowRight :: Key
key_ArrowRight = defaultKey "ArrowRight"

key_ArrowLeft :: Key
key_ArrowLeft = defaultKey "ArrowLeft"

key_ArrowDown :: Key
key_ArrowDown = defaultKey "ArrowDown"

key_ArrowUp :: Key
key_ArrowUp = defaultKey "ArrowUp"

key_ShiftArrowRight :: Key
key_ShiftArrowRight = key_ArrowRight { shift = true }

key_ShiftArrowLeft :: Key
key_ShiftArrowLeft = key_ArrowLeft { shift = true }

key_ShiftArrowDown :: Key
key_ShiftArrowDown = key_ArrowDown { shift = true }

key_ShiftArrowUp :: Key
key_ShiftArrowUp = key_ArrowUp { shift = true }

key_Escape :: Key
key_Escape = defaultKey "Escape"

key_Backspace :: Key
key_Backspace = defaultKey "Backspace"

key_Space :: Key
key_Space = defaultKey " "

key_Period :: Key
key_Period = defaultKey "."

key_enlambda :: Key
key_enlambda = (defaultKey "l") { ctrl = true }

key_enlet :: Key
key_enlet = (defaultKey "f") { ctrl = true }

key_enapp :: Key
key_enapp = key_Space { shift = true }

key_enarg :: Key
key_enarg = key_Space

key_unwrap :: Key
key_unwrap = (defaultKey "d") { ctrl = true }

key_dig :: Key
key_dig = (defaultKey "d") { ctrl = true, shift = true }
