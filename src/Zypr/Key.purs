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

validIdStrings :: Array String
validIdStrings =
  String.split (String.Pattern " ")
    $ "a b c d e f g h i j k l m n o p q r s t u v w x y z "
    <> "0 1 2 3 4 5 6 7 8 9 "
    <> "- = ; ' , . / ! @ # $ % ^ & * _ + : < > ?"

isValidIdModificationKey :: Key -> Boolean
isValidIdModificationKey key =
  and
    [ not key.meta
    , not key.ctrl
    , not key.alt
    , String.toLower key.label `elem` validIdStrings
    ]

isValidQueryModificationKey :: Key -> Boolean
isValidQueryModificationKey key =
  and
    [ not key.meta
    , not key.ctrl
    , not key.alt
    , String.toLower key.label `elem` validIdStrings
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

key_Enter :: Key
key_Enter = defaultKey "Enter"

key_Backspace :: Key
key_Backspace = defaultKey "Backspace"

key_ShiftBackspace :: Key
key_ShiftBackspace = (defaultKey "Backspace") { shift = true }

key_CtrlBackspace :: Key
key_CtrlBackspace = (defaultKey "Backspace") { ctrl = true }

key_Tab :: Key
key_Tab = defaultKey "Tab"

key_ShiftTab :: Key
key_ShiftTab = (defaultKey "Tab") { shift = true }

key_Space :: Key
key_Space = defaultKey " "

key_CtrlSpace :: Key
key_CtrlSpace = key_Space { ctrl = true }

key_ShiftCtrlSpace :: Key
key_ShiftCtrlSpace = key_Space { ctrl = true, shift = true }

key_ToggleConsoleVisible :: Key
key_ToggleConsoleVisible = (defaultKey "`") { ctrl = true }

key_Period :: Key
key_Period = defaultKey "."

key_Slash :: Key
key_Slash = defaultKey "/"

key_enlambda :: Key
key_enlambda = (defaultKey "l") { ctrl = true }

key_enlet :: Key
key_enlet = (defaultKey "d") { ctrl = true }

key_enapp :: Key
key_enapp = key_Space { shift = true }

key_enarg :: Key
key_enarg = key_Space

key_copy :: Key
key_copy = (defaultKey "c") { ctrl = true }

key_cut :: Key
key_cut = (defaultKey "x") { ctrl = true }

key_paste :: Key
key_paste = (defaultKey "v") { ctrl = true }

key_undo :: Key
key_undo = (defaultKey "z") { ctrl = true }
