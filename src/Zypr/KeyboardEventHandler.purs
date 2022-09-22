module Zypr.KeyboardEventHandler where

import Prelude
import Zypr.Key
import Effect (Effect)
import Effect.Console as Console
import React (ReactThis)
import Undefined (undefined)
import Web.Event.Event (Event, preventDefault)
import Zypr.EditorEffect (EditorEffect, runEditorEffect)
import Zypr.EditorEffect as EditorEffect
import Zypr.EditorTypes (EditorState, EditorProps)
import Zypr.Key as Key

keyboardEventHandler :: ReactThis EditorProps EditorState -> Event -> Effect Unit
keyboardEventHandler this event = do
  when (shouldPreventDefault event) do
    preventDefault event
  let
    key = Key.fromEvent event
  Console.log $ "key down: " <> show key
  runEditorEffect this $ handleKey key

shouldPreventDefault :: Event -> Boolean
shouldPreventDefault event = undefined

handleKey :: Key.Key -> EditorEffect Unit
handleKey key
  | key == key_ArrowLeft = EditorEffect.stepLeft
  | key == key_ArrowRight = EditorEffect.stepRight
  | key == key_ArrowDown = EditorEffect.stepDown
  | key == key_ArrowUp = EditorEffect.stepUp
  | otherwise = pure unit
