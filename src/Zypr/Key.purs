module Zypr.Key where

import Prelude
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

key_Period :: Key
key_Period = defaultKey "."
