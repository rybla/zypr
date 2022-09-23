module Zypr.KeyboardEventHandler where

import Prelude
import Zypr.Key
import Data.Array (elem)
import Data.Foldable (or)
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
  | key == key_ArrowLeft = EditorEffect.stepPrev
  | key == key_ArrowRight = EditorEffect.stepNext
  | key == key_ArrowDown = EditorEffect.stepDown
  | key == key_ArrowUp = EditorEffect.stepUp
  | otherwise = pure unit
