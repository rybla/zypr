-- interface for modifying strings via Keys
module Zypr.ModifyString where

import Prelude
import Zypr.Key
import Data.Array (elem)
import Data.Foldable (and, or)
import Data.Maybe (Maybe(..))
import Data.String as String

isValidStringModificationKey :: Key -> Boolean
isValidStringModificationKey key =
  or
    [ key
        `elem`
          [ key_CtrlBackspace
          , key_Enter
          , key_Escape
          , key_Backspace
          ]
    , and
        [ not key.meta
        , not key.ctrl
        , not key.alt
        , String.toLower key.label `elem` validIdStrings
        ]
    ]

data StringModificationResult
  = Submit
  | Escape

modifyStringViaKey :: Key -> String -> String
modifyStringViaKey key string = (modifyStringViaKeyWithResult key string).string

modifyStringViaKeyWithResult ::
  Key ->
  String ->
  { mb_result :: Maybe StringModificationResult
  , string :: String
  }
modifyStringViaKeyWithResult key string
  | key == key_CtrlBackspace = { string: "", mb_result: Nothing }
  | key == key_Enter = { string, mb_result: Just Submit }
  | key == key_Escape = { string, mb_result: Just Escape }
  | key == key_Backspace = { string: String.take (max 0 $ String.length string - 1) string, mb_result: Nothing }
  | string == " " = { string, mb_result: Nothing }
  | otherwise = { string: string <> key.label, mb_result: Nothing }

-- removeLeadingSpace :: String -> String
-- removeLeadingSpace str = case String.uncons str of
--   Just { head: cp, tail: str' }
--     | cp == String.codePointFromChar ' ' -> str'
--     | otherwise -> str
--   _ -> str
