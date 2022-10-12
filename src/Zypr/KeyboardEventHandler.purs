module Zypr.KeyboardEventHandler where

import Prelude
import Zypr.Key
import Control.Monad.State (get)
import Data.Array (elem)
import Data.Foldable (or, sequence_)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console as Console
import React (ReactThis)
import Web.Event.Event (Event, preventDefault)
import Zypr.EditorEffect (EditorEffect, runEditorEffect)
import Zypr.EditorEffect as EditorEffect
import Zypr.EditorTypes (CursorMode, EditorMode(..), EditorProps, EditorState, SelectMode)
import Zypr.Key as Key
import Zypr.ModifyString (isValidStringModificationKey, modifyStringViaKey)

keyboardEventHandler :: ReactThis EditorProps EditorState -> Event -> Effect Unit
keyboardEventHandler this event = do
  let
    key = Key.fromEvent event
  when (shouldPreventDefault key) do
    preventDefault event
  Console.log $ "key down: " <> show key
  runEditorEffect this $ handleKey key

shouldPreventDefault :: Key.Key -> Boolean
shouldPreventDefault key =
  or
    [ key
        `elem`
          [ key_enlambda
          , key_enlet
          , key_enapp
          , key_enarg
          ]
    , isValidIdModificationKey key
    , key.label
        `elem`
          ( _.label
              <$> [ key_ArrowLeft
                , key_ArrowRight
                , key_ArrowDown
                , key_ArrowUp
                , key_Space
                , key_Tab
                , key_Backspace
                , key_Slash
                ]
          )
    ]

handleKey :: Key.Key -> EditorEffect Unit
handleKey key
  -- misc
  | key == key_ToggleConsoleVisible = EditorEffect.toggleConsoleVisible
  -- cursor movement
  | key == key_ArrowLeft = EditorEffect.arrowleft
  | key == key_ArrowRight = EditorEffect.arrowright
  -- select movement
  | key == key_ShiftArrowLeft = EditorEffect.shiftArrowleft
  | key == key_ShiftArrowRight = EditorEffect.shiftArrowright
  -- enter, escape
  | key == key_Period = EditorEffect.enterSelect false
  | key == key_Escape = EditorEffect.escape
  -- tab to next/prev hole
  | key == key_Tab = EditorEffect.toggleIndent
  | key == key_Tab = EditorEffect.stepNextHole
  | key == key_ShiftTab = EditorEffect.stepPrevHole
  -- copy, cut, paste
  | key == key_copy = EditorEffect.copy
  | key == key_cut = EditorEffect.cut
  | key == key_paste = EditorEffect.paste
  -- undo
  | key == key_undo = EditorEffect.undo
  -- special keys
  | key == key_Backspace = EditorEffect.backspace
  | key == key_CtrlBackspace = EditorEffect.backspaceSuper
  | key == key_CtrlSpace = EditorEffect.stepNextHole
  | key == key_ShiftCtrlSpace = EditorEffect.stepPrevHole
  | key == key_Space = EditorEffect.space
  | key == key_Enter = EditorEffect.enter
  -- query
  | isValidStringModificationKey key = EditorEffect.keyinput key
  -- TODO: tmp disable for testing query
  -- -- modify term
  -- | key == key_enlambda = EditorEffect.enlambda
  -- | key == key_enlet = EditorEffect.enlet
  -- | key == key_enapp = EditorEffect.enapp
  -- | key == key_enarg = EditorEffect.enarg
  -- -- modify Id
  -- | isValidIdModificationKey key = EditorEffect.editId key.label
  -- | key == key_Backspace = EditorEffect.backspace
  -- | key == key_ShiftBackspace = EditorEffect.backspace'
  -- | key == key_CtrlBackspace = EditorEffect.backspaceSuper
  -- not handled
  | otherwise = pure unit
