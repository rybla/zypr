module Zypr.KeyboardEventHandler where

import Prelude
import Zypr.Key
import Data.Array (elem)
import Data.Foldable (or, sequence_)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console as Console
import React (ReactThis)
import Web.Event.Event (Event, preventDefault)
import Zypr.EditorEffect (EditorEffect, runEditorEffect)
import Zypr.EditorEffect as EditorEffect
import Zypr.EditorTypes (EditorState, EditorProps)
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
    [ key `elem` [ key_ArrowLeft, key_ArrowRight, key_ArrowDown, key_ArrowUp ]
    ]

handleKey :: Key.Key -> EditorEffect Unit
handleKey key
  -- cursor movement
  | key == key_ArrowLeft = sequence_ [ EditorEffect.exitSelect, EditorEffect.stepPrev ]
  | key == key_ArrowRight = sequence_ [ EditorEffect.exitSelect, EditorEffect.stepNext ]
  | key == key_ArrowDown = sequence_ [ EditorEffect.exitSelect, EditorEffect.stepDown ]
  | key == key_ArrowUp = sequence_ [ EditorEffect.exitSelect, EditorEffect.stepUp ]
  -- select movement
  | key == key_ShiftArrowLeft = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepPrev ]
  | key == key_ShiftArrowRight = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepNext ]
  | key == key_ShiftArrowDown = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepDown ]
  | key == key_ShiftArrowUp = sequence_ [ EditorEffect.enterSelect, EditorEffect.stepUp ]
  -- select
  | key == key_Period = EditorEffect.enterSelect
  | key == key_Escape = EditorEffect.exitSelect
  | otherwise = pure unit
