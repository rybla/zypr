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
          [ key_ArrowLeft
          , key_ArrowRight
          , key_ArrowDown
          , key_ArrowUp
          , key_Space
          , key_Backspace
          , key_enlambda
          , key_enlet
          , key_enapp
          , key_enarg
          , key_unwrap
          , key_dig
          ]
    ]

handleKey :: Key.Key -> EditorEffect Unit
handleKey key
  -- cursor movement
  | key == key_ArrowLeft = sequence_ [ EditorEffect.escapeSelect, EditorEffect.stepPrev ]
  | key == key_ArrowRight = sequence_ [ EditorEffect.escapeSelect, EditorEffect.stepNext ]
  | key == key_ArrowDown = sequence_ [ EditorEffect.escapeSelect, EditorEffect.stepDown ]
  | key == key_ArrowUp = sequence_ [ EditorEffect.escapeSelect, EditorEffect.stepUp ]
  -- select movement
  | key == key_ShiftArrowLeft = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepPrev ]
  | key == key_ShiftArrowRight = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepNext ]
  | key == key_ShiftArrowDown = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepDown ]
  | key == key_ShiftArrowUp = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepUp ]
  -- select
  | key == key_Period = EditorEffect.enterSelect
  | key == key_Escape = do
    state <- get
    case state.mode of
      CursorMode _ -> EditorEffect.escapeCursor
      SelectMode _ -> EditorEffect.escapeSelect
      _ -> pure unit
  -- copy, cut, paste
  | key == key_copy = EditorEffect.copy
  | key == key_cut = EditorEffect.cut
  | key == key_paste = EditorEffect.paste
  -- modify term
  | key == key_enlambda = EditorEffect.enlambda
  | key == key_enlet = EditorEffect.enlet
  | key == key_enapp = EditorEffect.enapp
  | key == key_enarg = EditorEffect.enarg
  | key == key_unwrap = EditorEffect.unwrap
  | key == key_dig = EditorEffect.dig
  | isAlphaNum key = EditorEffect.editId key.label
  | key == key_Backspace =
    EditorEffect.isEditable
      >>= case _ of
          true -> EditorEffect.editId key.label
          false -> EditorEffect.unwrap
  | otherwise = pure unit
